{application, hello,
    [{description, "Hello world tcp server"},
     {vsn, "0.0.1"},
     {modules, [hello]},
     {registered, []},
     {applications, [kernel, stdlib]},
     {mod, {hello, []}},
     {start_phases, []}
    ]}.
