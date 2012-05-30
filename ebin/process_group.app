%% This is the application resource file (.app file) for the 'base'
%% application.
{application, process_group, [
    {description, "Process grouping application."},
    {vsn, "1.0"},
    {modules, [
        process_group_app,
        process_group_supervisor,
        process_group
    ]},
    {registered,[]},
    {applications, [
        kernel,
        stdlib,
        prox_misc
    ]},
    {mod, {process_group_app,[]}}
]}.