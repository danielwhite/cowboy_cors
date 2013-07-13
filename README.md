A [cowboy](https://github.com/extend/cowboy) middleware implementation
of the W3C Cross-Origin Resource Sharing specification.

The [W3C Candidate Recommendation 29 January
2013](http://www.w3.org/TR/2013/CR-cors-20130129/) is the reference
for the current implementation.

Currently, the middleware only supports simple cross-origin requests
and actual requests.

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
| cors_allowed_origins   | `[]`                      |
| cors_allow_credentials | `false`                   |
| cors_exposed_headers   | `[]`                      |
| cors_allowed_headers   | `[]`                      |
| cors_allowed_methods   | `[]`                      |

# Todo

* Allow wildcard response for `cors_allowed_credentials`.

* Provide callback to set the `Access-Control-Max-Age` header.

* Simplify callback names for policies (more like cowboy_rest).

* Allow individual handlers to provide policy, rather than just a
  global policy.
