{node, main, 'main@DESKTOP-N3ROL9Q.lan'}.
{node, backup, 'backup@DESKTOP-N3ROL9Q.lan'}.

{init, [main, backup], [{node_start, [{monitor_master, true}]}]}.

{alias, root, "./test"}.

{logdir, all_nodes, "./logs/"}.
{logdir, master, "./logs/"}.

{groups, [main], root, auction_client_dist_SUITE, main}.
{groups, [backup], root, auction_client_dist_SUITE, backup}.