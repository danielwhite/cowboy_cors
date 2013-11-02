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

## Descriptions

### allowed_origins

> * Value type: '*' | [binary()]
> * Default value: []

Return the list of [allowed
origins](http://www.w3.org/TR/2013/CR-cors-20130129/#access-control-allow-origin-response-header).

This may optionally return the atom `'*'` to allow requests from any
origin.  In this case, the value of the `Access-Control-Allow-Origin`
header will be the origin of the request rather than a value of "*".

### allow_credentials

> * Value type: boolean()
> * Default value: false

Return whether the resource [supports user
credentials](http://www.w3.org/TR/2013/CR-cors-20130129/#supports-credentials).

### exposed_headers

> * Value type: [binary()]
> * Default value: []

Return a [list of header
names](http://www.w3.org/TR/2013/CR-cors-20130129/#list-of-exposed-headers)
that can be exposed to the client.

### allowed_headers

> * Value type: [binary()]
> * Default value: []

Return a [list of header
names](http://www.w3.org/TR/2013/CR-cors-20130129/#list-of-headers)
that are supported by the resource.

### allowed_methods

> * Value type: [binary()]
> * Default value: []

Return a [list of
methods](http://www.w3.org/TR/2013/CR-cors-20130129/#list-of-methods)
that the resource supports.

### max_age

> * Value type: non_neg_integer() | undefined
> * Default value: undefined

Return the [maximum
time](http://www.w3.org/TR/2013/CR-cors-20130129/#http-access-control-max-age)
(in seconds) that the results of a preflight request can be cached by
the client.