%% This is the application resource file (.app file) for the 'base'
%% application.
{application, ppool,
[{description, "ppool " },
{vsn, "1.0.0" },
{modules, 
	  [ppool, ppool_serv, ppool_sup, ppool_supersup, ppool_worker_sup]},
{registered,[ppool]},
{applications, []},
{mod, {ppool,[]}},
{start_phases, []}
]}.
