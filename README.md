# Syslog Client in Ada

The package collection rooted at `Syslog` provided here is a library that
builds messages according to the BSD Syslog protocol, as defined by
[RFC 5424][1] or the older [RFC 3164][2].

While there are several log frameworks and bindings in Ada, very well
summarized by Thomas Løcke in [Logging options for Ada][3], they either
have no syslog support, or only have a binding to host-provided variadic 
`syslog` C function.

On the contrary, this library takes care of message formatting without any
support from the host, and sends them either via user-provided callback
or sample transport provided (e.g. GNAT UDP socket).

This is especially useful for small embedded applications with network
support, that can't afford to log locally.

[1]: http://tools.ietf.org/html/rfc3164 "RFC 3164 - The BSD syslog Protocol"
[2]: http://tools.ietf.org/html/rfc5424 "RFC 5424 - The Syslog Protocol"
[3]: http://ada-dk.org/2011/11/logging-options-for-ada/ "Logging options for Ada"
