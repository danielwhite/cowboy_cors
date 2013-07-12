{application,example,
             [{description,"Example CORS policy via cowboy middleware."},
              {vsn,"1"},
              {registered,[]},
              {applications,[kernel,stdlib,cowboy]},
              {mod,{example_app,[]}},
              {env,[]},
              {modules,[example,example_app,example_handler,example_policy,
                        example_sup]}]}.
