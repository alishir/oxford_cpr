{node, master, 'ct@DESKTOP-N3ROL9Q.lan'}.
{node, client, 'client@DESKTOP-N3ROL9Q.lan'}.
{node, auction_server, 'auction_server@DESKTOP-N3ROL9Q.lan'}.

{init, 
 auction_server, 
 [{node_start, [{monitor_master, true},
                {erl_flags, "-pa ../auction_server/ebin/ "
                            "-config ./configs/auction_server.config"}]}]}.
{init, 
 client, 
 [{node_start, [{monitor_master, true},
                {erl_flags, "-pa ../auction_client/ebin/ "
                            "-config ./configs/client.config"}]}]}.

{alias, root, "./"}.

{logdir, all_nodes, "./simulation_logs/"}.
{logdir, master, "./simulation_logs/"}.

{groups, [client], root, auction_simulation_SUITE, client}.
{groups, [auction_server], root, auction_simulation_SUITE, auction_server}.