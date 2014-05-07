-define(surround_if_colored(Prefix, X, Suffix),
        begin
        case application:get_env(topcat, colored) of
            {ok, ["false"]} -> X;
            _ -> Prefix ++ X ++ Suffix
        end
    end).
-define(red(X), ?surround_if_colored("\e[0;91m", X, "\e[0m")).
-define(green(X), ?surround_if_colored("\e[0;92m", X, "\e[0m")).
-define(yellow(X), ?surround_if_colored("\e[0;93m", X, "\e[0m")).
-define(cyan(X), ?surround_if_colored("\e[0;96m", X, "\e[0m")).
-define(grey(X), ?surround_if_colored("\e[0;37m", X, "\e[0m")).
