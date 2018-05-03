%% This is the application resource file (.app file) for the 'base'
%% application.
{application, template,
[{description, "template for services " },
{vsn, "1.2.3" },
{modules, 
	  [template,template_lib,template_server]},
{registered,[template]},
{applications, []},
{mod, {template,[]}},
{start_phases, []}
]}.
