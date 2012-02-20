-module(rebar_scm_plugin).

-export([
	scm/2
]).

-include_lib("rebar.hrl").


%%
% url() ::= {protocol, [Path]}

as_url(UrlString) ->
	Sep = "://",
	L = length(Sep),
	case string:str(UrlString, Sep) of
		0 -> {error, {invalid_url, UrlString}};
		Index ->
			ProtocolStr = lists:sublist(UrlString, 1, Index - 1),
			PathStr = lists:sublist(UrlString, Index + L, length(UrlString) - Index - L + 1),
			{ok, {list_to_atom(ProtocolStr), string:tokens(PathStr, "/")}}
	end.


as_string({Protocol, PathParts})  ->
	atom_to_list(Protocol) ++ "://" ++ string:join(PathParts, "/").


% copied from http://hyperthunk.github.com/rebar-plugin-tutorial/part-2-plugin-anatomy/index.html
is_base_dir() ->
    rebar_utils:get_cwd() == rebar_config:get_global(base_dir, undefined).

scm(Config, AppFile) ->
	?DEBUG("scm(~p, ~s)~n", [Config, AppFile]),
	case is_base_dir() of
		true -> 
			Scm = rebar_config:get_local(Config, scm, undefined),
			case Scm of
				undefined -> 
					?ERROR("No SCM information provided in rebar.config file~n", []),
					?FAIL;
				_ -> ok
			end,
			Tag = case rebar_config:get_global(tag, undefined) of
				undefined -> 
					Vsn = rebar_app_utils:app_vsn(AppFile),
					?INFO("No tag specified. Tagging with application version [~s]", [Vsn]),
					Vsn;
				Other -> 
					?INFO("Tagging with tag ~s~n", [Other]),
					Other
			end,
			tag(Scm,Tag);
		false -> 
			?DEBUG("Ignoring dependency ",[])
	end,
	ok.

tag({svn, UrlStr}, Tag) when is_list(UrlStr) ->
	{ok, {Protocol, Path}} = as_url(UrlStr),
	TargetPath = case lists:last(Path) of
		% get rid of /trunk and append /tags/[Tag]
		"trunk" -> lists:append(lists:sublist(Path, length(Path) - 1), ["tags",Tag]);
		_ -> lists:append(Path, ["tags", Tag])
	end,
	rebar_utils:sh(?FMT("svn copy -m \"~s\" ~s ~s", ["Tagging " ++ Tag, UrlStr, as_string({Protocol, TargetPath})]), []);

tag({git, _Url}, Tag) ->
	?INFO("Tagging with Git ~s", [Tag]),
	rebar_utils:sh(?FMT("git tag ~s", [Tag]));


tag({Scm, _}, _Tag) ->
	?ERROR("Unknown scm: ~p", [Scm]).

