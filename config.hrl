-define(SOCK_OPTS, [binary, {active, true},
                            {sndbuf, 11000},
                            {recbuf, 11000}
                   ]).
-define(PORT, 1337).
-define(HOST, "128.199.56.84").
% -define(HOST, "localhost").
-define(TIMEOUT, 10000).
-define(PACKET_SIZE, 1000).
