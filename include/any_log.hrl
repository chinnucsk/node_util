-define(log(Level, Format, Args),
        any_log:log(Level, io_lib:format("~p ~s ~n~p:~p~n" ++ Format,[self(), node(), ?MODULE, ?LINE|Args]))).
