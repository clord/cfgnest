cfgnest
=======

Hosts a reverse-lookup configuration database for other nodes on the network

This tool replaces an internal tool I help maintain for the compiler team of my employer. 
This version is several orders faster than the older ruby-based code that it replaces. 
Keys are matched from the leaf up to the root, allowing for specialization of configuration. 
This model enables generic keys placed at the root, and specialized variables defined in leaves. 

Supports substitution in values (syntax: `${Key}`), which are resolved at the same context. 

homepage and more discussion:

http://c.lord.ac/posts/2013-08-01-cfgnest-configuration-server.html
