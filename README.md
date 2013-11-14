[![Build Status](https://travis-ci.org/danielwhite/cowboy_cors.png?branch=master)](https://travis-ci.org/danielwhite/cowboy_cors)

A [cowboy](https://github.com/extend/cowboy) middleware implementation
of the W3C Cross-Origin Resource Sharing specification.

The [W3C Candidate Recommendation 29 January
2013](http://www.w3.org/TR/2013/CR-cors-20130129/) is the reference
for the current implementation.

# Configuration

The CORS middleware requires the `cors_policy` value.  This value
indicates the name of the module implementing the `cowboy_cors_policy`
behaviour.

# Policy Modules

A number of optional callbacks can be implemented in the policy
module.  Each callback should accept the Req object and the State as
arguments, and return a three-tuple of the from `{Value, Req, State}`.

| Callback name          | Default value             |
| ---------------------- | ------------------------- |
| allowed_origins        | `[]`                      |
| allow_credentials      | `false`                   |
| exposed_headers        | `[]`                      |
| allowed_headers        | `[]`                      |
| allowed_methods        | `[]`                      |

# Todo


* Provide callback to set the `Access-Control-Max-Age` header.
* Allow individual handlers to provide policy, rather than just a
  global policy.
