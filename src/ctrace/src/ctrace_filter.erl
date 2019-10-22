% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(ctrace_filter).
-include_lib("passage/include/opentracing.hrl").

-behaviour(passage_reporter).

-export([
    new/2,
    report/2,
    module/1
]).


new(OperationId, Module) ->
    {OperationId, Module}.


report({_OperationId, Module}, PSpan) ->
    Tags = passage_span:get_tags(PSpan),
    try
        case Module:match(Tags) of
            false ->
                ok;
            Actions ->
                lists:takewhile(fun(Action) -> Action(PSpan) end, Actions),
                ok
        end
    catch error:undef ->
        ok
    end.


module({_OperationId, Module}) ->
    Module.
