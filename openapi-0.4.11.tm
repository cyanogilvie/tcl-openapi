# TODO: generalize this into an OpenAPI code generator

package require rl_json
package require parse_args
package require chantricks
package require tcl::chan::variable

namespace eval ::openapi {
	namespace export *
	namespace ensemble create -prefixes no

	namespace eval helpers {
		proc longest l { #<<<
			set longest	0
			foreach e $l {
				if {[set len [string length $e]] > $longest} {
					set longest	$len
				}
			}
			set longest
		}

		#>>>
		proc indent {indent script} { #<<<
			join [lmap line [split $script \n] {
				format %s%s $indent $line
			}] \n
		}

		#>>>
	}

	namespace path {::parse_args ::rl_json ::chantricks ::openapi::helpers}

	namespace eval generate {
		namespace export *
		namespace ensemble create -prefixes no
		namespace path {::parse_args ::rl_json ::chantricks ::openapi::helpers}

		proc tm args { #<<<
			parse_args $args {
				-ziplet	{-boolean}
				in		{-required}
			} cfg

			set s	[dict get $cfg in]

			set ns			docker
			set ensembles	{}
			json foreach {path inf} [json extract $s paths] {
				json foreach {method def} $inf {
					foreach {k default} {
						produces	nothing?
						consumes	{}
					} {
						if {[json exists $def $k]} {
							set $k	[json get $def $k]
						} else {
							set $k	$default
						}
					}

					#puts "[json get $def operationId]: [string toupper $method] $path -> $produces"
					set pargs	{-server {-default http://[/var/run/docker.sock]} -response_headers {}}
					set in		{path {} query {} body {} header {}}
					if {[json exists $def parameters]} {
						json foreach param [json extract $def parameters] {
							foreach {k default} {
								type		??
							} {
								if {[json exists $param $k]} {
									set $k	[json get $param $k]
								} else {
									set $k	$default
								}
							}

							set argspec	{}
							if {$type eq "boolean"} {
								lappend argspec -boolean
							}

							if {
								[json exists $param required] &&
								[json get $param required] &&
								$type ne "boolean"
							} {
								lappend argspec	-required
							}

							lappend pargs -[json get $param name] $argspec

							dict lappend in [json get $param in] [json get $param name]

							if {[json get $param in] eq "body"} {
								set binary	0
								if {[json exists $param schema format] && [json get $param schema format] eq "binary"} {
									set binary	1
								}

								if {"application/json" in $consumes} {
									# Not binary
									set mimetype	application/json
								} else {
									# Otherwise, if any of the declared types are binary, treat as binary
									foreach m $consumes {
										switch -glob -- $m {
											text/* -
											application/json {
											}
											application/* -
											image/* -
											default {
												set binary	1
												set mimetype	$m
												break
											}
										}
									}
								}
							}

							#puts [format "\t-%-20s %-10s %-10s" [json get $param name] [json get $param in] $type]
						}
					}

					set longest_name	[longest [dict keys $pargs]]
					set tabs			[expr {max(3, int(ceil(($longest_name+1) / 4.0)))}]
					set procbody	{}
					append procbody "\tparse_args \$args \{" \n
					foreach {name argspec} $pargs {
						set padding	[expr {
							$tabs - int(floor([string length $name]/4.0))
						}]
						#puts "padding: ($padding), tabs: ($tabs), consumed: [expr {ceil([string length $name]/4.0)}], longest_name: $longest_name"
						append procbody "\t\t$name[string repeat \t $padding]{$argspec}" \n
					}
					append procbody "\t\} in" \n
					append procbody "" \n
					append procbody "\tset extra\t{}" \n
					if {[llength [dict get $in path]] > 0} {
						append procbody "\tset pathmap\t{}" \n
						foreach name [dict get $in path] {
							append procbody "\tlappend pathmap {{[list $name]}}\t\[urlencode rfc_urlencode -part path -- \[dict get \$in [list $name]\]\]" \n
						}
						append procbody "\tset path\t\[string map \$pathmap [list $path]\]" \n
					} else {
						append procbody "\tset path\t[list $path]" \n
					}
					append procbody "\tset query\t{}" \n
					foreach name [dict get $in query] {
						append procbody "\tif {\[dict exists \$in [list $name]\]} {lappend query [list $name]\t\[dict get \$in [list $name]\]}" \n
					}
					if {[llength [dict get $in header]] > 0} {
						append procbody "\tset headers\t{}" \n
						foreach name [dict get $in header] {
							append procbody "\tif {\[dict exists \$in [list $name]\]} {lappend headers [list $name]\t\[dict get \$in [list $name]\]}" \n
						}
						append procbody "\tlappend extra -headers \$headers"
					}

					switch -- [llength [dict get $in body]] {
						0 {}
						1 {
							if {$binary} {
								append procbody "\tlappend extra -body \[dict get \$in [list [lindex [dict get $in body] 0]]\]" \n
							} else {
								append procbody "\tlappend extra -body \[encoding convertto utf-8 \[dict get \$in [list [lindex [dict get $in body] 0]]\]\]" \n
							}
							append procbody "\tlappend headers Content-Type [list $mimetype]" \n
						}
						default {
							error "Multiple parameters target the body"
						}
					}

					append procbody "\tif \{\[dict exists \$in response_headers\]\} \{" \n
					append procbody "\t\tupvar 1 \[dict get \$in response_headers\] response_headers" \n
					append procbody "\t\}" \n

					append procbody "\treq \[dict get \$in server\] [list [string toupper $method]] \$path\[urlencode encode_query \$query\] {*}\$extra" \n

					set op	[json get $def operationId]
					if {[json exists $def tags]} {
						set tag	[json get $def tags 0]

						if {[string range $op 0 [string length $tag]-1] eq $tag} {
							# Strip $tag off the front
							#puts -nonewline "op ($op) -> "
							set op	[string range $op [string length $tag] end]
							#puts "($op)"
							if {$op eq ""} {
								set op	$tag
								set tag	""
							}
						}
					} else {
						set tag	{}
					}

					# Camel to underscores
					foreach v {tag op} {
						#puts -nonewline "$v ([set $v]) -> "
						set $v	[join [lmap e [regexp -all -inline {[A-Za-z][a-z]+} [set $v]] {string tolower $e}] _]
						#puts "([set $v])"
					}

					dict lappend ensembles $tag [list $op $procbody]
				}
			}

			try {
				with_chan h {
					# tcl::chan::variable can only work with global variables
					tcl::chan::variable ::_openapi_out
				} {
					if {[dict get $cfg ziplet]} {
						puts $h "apply \{\{\} \{ # Code is gzipped and appended to this script"
						puts $h "	set h \[open \[info script\] rb\]"
						puts $h "	try \{"
						puts $h "		set data	\[read \$h\]"
						puts $h "		set eof		\[string first \\u1A \$data\]"
						puts $h "		eval \[encoding convertfrom utf-8 \[zlib gunzip \[string range \$data \$eof+1 end\]\]\]"
						puts $h "	\} finally \{"
						puts $h "		close \$h"
						puts $h "	\}"
						puts $h "\}\}"
						puts -nonewline $h \u1A
						zlib push gzip $h -level 9
					}
					puts $h "package require rl_json"
					puts $h "package require parse_args"
					puts $h "package require rl_http 1.8"
					puts $h "package require urlencode"
					puts $h ""
					puts $h "namespace eval [list ::$ns] \{"
					puts $h "	namespace eval helpers \{"
					puts $h "		proc req \{server method path args\} \{ #<<<"
					puts $h "			upvar 1 response_headers response_headers  http_status http_status"
					puts $h "			rl_http instvar h \$method \$server\$path \{*\}\$args"
					puts $h "			set response_headers	\[\$h headers\]"
					puts $h "			set http_status			\[\$h code\]"
					puts $h "			switch -glob -- \[\$h code\] \{"
					#puts $h "				2* \{\$h body\}"
					puts $h "				2* \{"
					puts $h "					set body \[\$h body\]"
					puts $h "					if \{\[info exists ::tcl_interactive\] && \$::tcl_interactive\} \{"
					puts $h "						catch \{"
					puts $h "							set hdrs	\[\$h headers\]"
					puts $h "							if \{\[dict exists \$hdrs content-type\]\} \{"
					puts $h "								switch -regexp -- \[lindex \[dict get \$hdrs content-type\] 0\] \{"
					puts $h "									\{^(text|application)/json\[\[:>:\]\]\} - \{^\[a-zA-Z0-9_-\]+/\[a-zA-Z0-9_-\]\\+json\[\[:>:\]\]\} \{set body \[json pretty \$body\]\}"
					puts $h "									\{^(text|application)/xml\[\[:>:\]\]\} - \{^\[a-zA-Z0-9_-\]+/\[a-zA-Z0-9_-\]\\+xml\[\[:>:\]\]\} \{dom parse \$body doc; try \{\[\$doc documentElement\] asXML\} on ok body \{\} finally \{\$doc delete\}\}"
					puts $h "								\}"
					puts $h "							\}"
					puts $h "						\}"
					puts $h "					\}"
					puts $h "					set body"
					puts $h "				\}"
					puts $h "				default \{"
					puts $h "					try \{"
					puts $h "						json get \[\$h body\] message"
					puts $h "					\} on ok errmsg \{"
					puts $h "						throw \[list DOCKER HTTP \[\$h code\]\] \$errmsg"
					puts $h "					\} on error \{\} \{"
					puts $h "						throw \[list DOCKER HTTP \[\$h code\]\] \[\$h body\]"
					puts $h "					\}"
					puts $h "				\}"
					puts $h "			\}"
					puts $h "		\}\n"
					puts $h "		#>>>\n"
					puts $h "	\}\n"
					puts $h "\tnamespace path \{"
					puts $h "\t\t::parse_args"
					puts $h "\t\t::rl_json"
					puts $h "\t\t[list ::${ns}::helpers]"
					puts $h "\t\}"

					set ensemble_map	{}
					dict for {tag procs} $ensembles {
						if {$tag ne ""} {
							dict lappend ensemble_map {} $tag _$tag
							puts $h "\tnamespace eval [list _$tag] \{ #<<<"
							puts $h "\t\tnamespace path \{"
							puts $h "\t\t\t::parse_args"
							puts $h "\t\t\t::rl_json"
							puts $h "\t\t\t[list ::${ns}::helpers]"
							puts $h "\t\t\}"
							puts $h ""
						}
						foreach proc $procs {
							lassign $proc op procbody
							dict lappend ensemble_map $tag $op _$op
							set procscript "proc _$op args { #<<<\n$procbody}\n\n#>>>"
							puts $h [indent [string repeat \t [expr {$tag eq "" ? 1 : 2}]] $procscript]
						}
						if {$tag ne ""} {
							if {[dict exists $ensemble_map $tag]} {
								puts $h "\t\tnamespace ensemble create -prefixes no -map [list [dict get $ensemble_map $tag]]"
							}
							puts $h "\t\}\n\n\t#>>>"
						}
					}
					if {[dict exists $ensemble_map {}]} {
						puts $h "\tnamespace ensemble create -prefixes no -map [list [dict get $ensemble_map {}]]"
					}
					puts $h "\}\n"
					# Hook interactive mode tab completion to the ensemble handler
					puts $h "namespace eval ::tclreadline \{proc [list complete($ns)] \{text start end line pos mod\} \{try \{package require tclreadline::complete::ensemble\} on error \{\} \{return\}; tailcall ::tclreadline::complete::ensemble ::[list $ns] \$text \$start \$end \$line \$pos \$mod\}\}"
					puts $h ""
					puts $h "# vim\: ft=tcl foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4"
				}

				set ::_openapi_out
			} finally {
				unset -nocomplain ::_openapi_out
			}
		}

		#>>>
	}
}

# vim: ft=tcl foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4
