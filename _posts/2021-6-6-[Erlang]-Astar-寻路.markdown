---
layout:     post
title:      "[Erlang] Astar 寻路算法"
subtitle:   "Erlang"
date:       2021-06-02 20:00:00
author:     "Shaocong"
header-img: "img/leetcode-bg.jpg"
catalog: true
tags:
    - Erlang
    - 算法
---

#### Astar 寻路算法
> A star算法也叫A星(A*)算法，这是一种在图形平面上，有多个节点的路径，求出最低通过成本的算法。常用于游戏中的NPC的移动计算，或网络游戏的BOT的移动计算上.该算法综合了最良优先搜索和Dijkstra算法的优点：在进行启发式搜索提高算法效率的同时，可以保证找到一条最优路径（基于评估函数）。


![img](/img/in-post/astar_1.webp)

在A star算法中，如果我们要计算两点之间的距离，例如上图中起点到终点的距离，这时我们把两点之间的直线距离用f(n)表示，而两点之间的水平距离+竖直距离=曼哈顿距离(这个后面会用到，使用曼哈顿距离可以提高运算的速度，如果使用f(n)那么需要不断的计算三角函数)

![img](/img/in-post/astar_2.webp)

如图，首先假设我们要从绿色的点走到红色的点，蓝色的点表示障碍物。

那么：首先我们定义两个队列open和close，open表示正在处理或者准备处理的点，close表示已经处理过或者不处理的点

* 绿色点周围的8个点就是我们第一步可以走的位置
* 首先我们假设左边的水平方向和竖直方向的距离都是10，那么45度方向距离就是14(即√200)，
* 所以我们在每个方块的左下角标一个数字g(n)，它代表这个方块距离起始点的距离
* 然后在每个方块的右下角标一个数字h(n)，它代表这个方块距离终点的曼哈顿距离(先不考虑障碍物)。
* 所以我们可以大致估算出，每个方块所在路径到终点的大致距离，就是f(n)=g(n)+h(n)
* 现在我们将起始点周围的8个点加入到open队列中去，表示接下来要处理这8个点，然后将起始点加到close队列中，表示起始点已经处理过了
* 然后我们依照f(n)对open队列进行从小到大进行排序，取出f(n)最小的那个点，当作新的起始点，重复上面1-7步，直到到达终点，就可以得到下图

![img](/img/in-post/astar_3.webp)

参考：https://www.jianshu.com/p/950233af52df

##### Erlang实现
```erlang
-module(pathFinding).

%% 每个坐标格子大小
-define(ASTAR_MAP_WIDTH, 10).
%% 每个格子中心对角线距离 √10 * 10 + 10 * 10 ~= 14
-define(ASTAR_MAP_DIAGONAL, 14).

-record(astar_point, {pos = {0, 0}, parent = {0, 0}, g = 0, h = 0, f = 0}).
%% pos x,y坐标
%% g 此位置距离起点的距离
%% h 此位置距离终点的曼哈顿距离（不考虑障碍物），曼哈顿距离 = 水平距离 + 竖直距离
%% f 此位置到达终点的大致距离 f = g + h
%% parent 记录父节点：起点到该点最短距离的前一个点

-export([astar/8]).

%% moveSpeed 移动速度（每次移动最大格子数）， 用于构造简化路径，若moveSpeed=1则不构造
astar(CannotStandPoints, Width, Height, StartPosX, StartPosY, TargetPosX, TargetPosY, MoveSpeed) ->
    CheckMapWH = Width > 0 andalso Height > 0,
    case CheckMapWH of
        true ->
            CannotStandPointsWithEdge = astar_create_edge(Width, Height, CannotStandPoints),
            CheckStartAndTarget = lists:member({StartPosX, StartPosY}, CannotStandPointsWithEdge) == false
              andalso lists:member({TargetPosX, TargetPosY}, CannotStandPointsWithEdge) == false,
            case CheckStartAndTarget of
                true ->
                    StartH = abs(StartPosX - TargetPosX) * ?ASTAR_MAP_WIDTH + abs(StartPosY - TargetPosY) * ?ASTAR_MAP_WIDTH,
                    StartPoint = #astar_point{pos = {StartPosX, StartPosY}, parent = {StartPosX, StartPosY}, h = StartH, f = StartH},
                    TargetPoint = #astar_point{pos = {TargetPosX, TargetPosY}},
                    Path = astar_find_path(StartPoint, TargetPoint, CannotStandPointsWithEdge, [StartPoint], []),
                    case lists:keyfind(TargetPoint#astar_point.pos, #astar_point.pos, Path) of
                        #astar_point{} ->
                            ResultPath = astar_find_path_result(Path, TargetPoint#astar_point.pos, []),
                            case MoveSpeed of
                                1 ->
                                    ResultPath;
                                _ ->
                                    astar_find_path_simple_result(ResultPath, MoveSpeed, [])
                            end;
                        _ ->
                            []
                    end;
                _ ->
                    []
            end;
        _ ->
            []
    end.

%% 创建地图边缘
astar_create_edge(Width, Height, CannotStandPoints) ->
    CannotStandPointsWithTBEdge = astar_create_edge_top_and_bottom(Width, Height, CannotStandPoints),
    astar_create_edge_left_and_right(Height, Width, CannotStandPointsWithTBEdge).

%% 创建地图上下边缘
astar_create_edge_top_and_bottom(-1, _, CannotStandPoints) ->
    CannotStandPoints;
astar_create_edge_top_and_bottom(Width, Height, CannotStandPoints) ->
    astar_create_edge_top_and_bottom(Width - 1, Height, [{0, Width}, {Height + 1, Width} | CannotStandPoints]).

%% 创建地图左右边缘
astar_create_edge_left_and_right(-1, _, CannotStandPoints) ->
    CannotStandPoints;
astar_create_edge_left_and_right(Height, Width, CannotStandPoints) ->
    astar_create_edge_left_and_right(Height - 1, Width, [{Height, 0}, {Height, Width + 1} | CannotStandPoints]).

astar_find_path(_StartPoint, _TargetPoint, _CannotStandPoints, [], CloseList) ->
    CloseList;
astar_find_path(StartPoint, TargetPoint, CannotStandPoints, [Point | OpenList], CloseList) ->
    case lists:keyfind(Point#astar_point.pos, #astar_point.pos, CloseList) of
        false ->
            NewOpenList = astar_add_neighbor(Point, StartPoint, TargetPoint, OpenList, CannotStandPoints, CloseList),
            CheckEnd = Point#astar_point.pos == TargetPoint#astar_point.pos,
            case CheckEnd of
                true ->
                    [Point | CloseList];
                _ ->
                    astar_find_path(StartPoint, TargetPoint, CannotStandPoints, NewOpenList, [Point | CloseList])
            end;
        _ ->
            astar_find_path(StartPoint, TargetPoint, CannotStandPoints, OpenList, CloseList)
    end.

%% 添加周围（八个方向）点到open列表中
astar_add_neighbor(Point, _StartPoint, TargetPoint, OpenList, CannotStandPoints, CloseList) ->
    {X, Y} = Point#astar_point.pos,
    %% 上
    OpenList1 = astar_add_neighbor_to_open(X - 1, Y, TargetPoint, OpenList, CloseList, CannotStandPoints, Point#astar_point.pos, Point#astar_point.g + ?ASTAR_MAP_WIDTH),
    %% 下
    OpenList2 = astar_add_neighbor_to_open(X + 1, Y, TargetPoint, OpenList1, CloseList, CannotStandPoints, Point#astar_point.pos, Point#astar_point.g + ?ASTAR_MAP_WIDTH),
    %% 左
    OpenList3 = astar_add_neighbor_to_open(X, Y - 1, TargetPoint, OpenList2, CloseList, CannotStandPoints, Point#astar_point.pos, Point#astar_point.g + ?ASTAR_MAP_WIDTH),
    %% 右
    OpenList4 = astar_add_neighbor_to_open(X, Y + 1, TargetPoint, OpenList3, CloseList, CannotStandPoints, Point#astar_point.pos, Point#astar_point.g + ?ASTAR_MAP_WIDTH),
    %% 左上
    OpenList5 = astar_add_neighbor_to_open(X - 1, Y - 1, TargetPoint, OpenList4, CloseList, CannotStandPoints, Point#astar_point.pos, Point#astar_point.g + ?ASTAR_MAP_DIAGONAL),
    %% 左下
    OpenList6 = astar_add_neighbor_to_open(X + 1, Y - 1, TargetPoint, OpenList5, CloseList, CannotStandPoints, Point#astar_point.pos, Point#astar_point.g + ?ASTAR_MAP_DIAGONAL),
    %% 右上
    OpenList7 = astar_add_neighbor_to_open(X - 1, Y + 1, TargetPoint, OpenList6, CloseList, CannotStandPoints, Point#astar_point.pos, Point#astar_point.g + ?ASTAR_MAP_DIAGONAL),
    %% 右下
    OpenList8 = astar_add_neighbor_to_open(X + 1, Y + 1, TargetPoint, OpenList7, CloseList, CannotStandPoints, Point#astar_point.pos, Point#astar_point.g + ?ASTAR_MAP_DIAGONAL),

    SortFun = fun(P1, P2) -> P1#astar_point.h < P2#astar_point.h end,
    lists:sort(SortFun, OpenList8).

astar_add_neighbor_to_open(X, Y, TargetPoint, OpenList, CloseList, CannotStandPoints, ParentPos, G) ->
    case lists:member({X, Y}, CannotStandPoints) of
        false ->
            case lists:keyfind({X, Y}, #astar_point.pos, CloseList) of
                false ->
                    case lists:keyfind({X, Y}, #astar_point.pos, OpenList) of
                        false ->
                            {TargetPosX, TargetPosY} = TargetPoint#astar_point.pos,
                            H = abs(X - TargetPosX) * ?ASTAR_MAP_WIDTH + abs(Y - TargetPosY) * ?ASTAR_MAP_WIDTH,
                            Point = #astar_point{pos = {X, Y}, parent = ParentPos, g = G, h = H, f = G + H},
                            [Point | OpenList];
                        OldPoint ->
                            CheckReplace = G < OldPoint#astar_point.g,
                            case CheckReplace of
                                true ->
                                    NewPoint = OldPoint#astar_point{parent = ParentPos, g = G, f = G + OldPoint#astar_point.h},
                                    lists:keyreplace({X, Y}, #astar_point.pos, OpenList, NewPoint);
                                _ ->
                                    OpenList
                            end
                    end;
                _ ->
                    OpenList
            end;
        _ ->
            OpenList
    end.

%% 构建路径结果
astar_find_path_result([], _, _) ->
    [];
astar_find_path_result(Path, Pos, PosList) ->
    case lists:keyfind(Pos, #astar_point.pos, Path) of
        #astar_point{parent = NextPos} ->
            case NextPos of
                Pos ->
                    [Pos | PosList];
                _ ->
                    astar_find_path_result(Path, NextPos, [Pos | PosList])
            end;
        _ ->
            PosList
    end.

astar_find_path_simple_result([], _MoveSpeed, PosList) ->
    lists:reverse(PosList);
astar_find_path_simple_result([{X, Y}], MoveSpeed, PosList) ->
    astar_find_path_simple_result([], MoveSpeed, [{X, Y} | PosList]);
astar_find_path_simple_result([{X1, Y1}, {X2, Y2}], MoveSpeed, PosList) ->
    astar_find_path_simple_result([], MoveSpeed, [{X2, Y2}, {X1, Y1} | PosList]);
astar_find_path_simple_result([{X1, Y1}, {X2, Y2}, {X3, Y3} | Path], MoveSpeed, PosList) ->
    CheckIsSameDis = (X2-X1)*(Y3-Y2) == (X3 - X2) * (Y2 - Y1),
    case CheckIsSameDis of
        true ->
            CheckMoveSpeed = MoveSpeed >= math:sqrt(math:pow(X1 - X3, 2) + math:pow(Y1 - Y3, 2)),
            case CheckMoveSpeed of
                true ->
                    astar_find_path_simple_result([{X1, Y1}, {X3, Y3} | Path], MoveSpeed, PosList);
                _ ->
                    astar_find_path_simple_result([{X2, Y2}, {X3, Y3} | Path], MoveSpeed, [{X1, Y1} | PosList])
            end;
        _ ->
            astar_find_path_simple_result([{X2, Y2}, {X3, Y3} | Path], MoveSpeed, [{X1, Y1} | PosList])
    end.
```

##### 测试代码
```erlang
-module(pathFinding_test).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

astar_test() ->
    % [0, 0, 0, 0, 0, 0, 0, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, s, 0, 1, 0, t, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, 0, 0, 0, 0, 0, 0]
    % [0, 0, 0, 0, 0, 0, 0, 0]
    CannotStandPoints = [{2, 5}, {3, 5}, {4, 5}],
    Width = 8,
    Height = 6,
    StartPosX = 3,
    StartPosY = 3,
    TargetPosX = 3,
    TargetPosY = 7,
    MoveSpeed = 1,
    Path = pathFinding:astar(CannotStandPoints, Width, Height, StartPosX, StartPosY, TargetPosX, TargetPosY, MoveSpeed),
    [{StartPosX, StartPosY}, {4, 4}, {5, 5}, {4, 6}, {TargetPosX, TargetPosY}] = Path,
    ok.

astar2_test() ->
    % [0, 0, 0, 0, 0, 0, 0, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, s, 0, 1, 0, t, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    CannotStandPoints = [{2, 5}, {3, 5}, {4, 5}, {5, 5}, {6, 5}],
    Width = 8,
    Height = 6,
    StartPosX = 3,
    StartPosY = 3,
    TargetPosX = 3,
    TargetPosY = 7,
    MoveSpeed = 1,
    Path = pathFinding:astar(CannotStandPoints, Width, Height, StartPosX, StartPosY, TargetPosX, TargetPosY, MoveSpeed),
    [{StartPosX, StartPosY}, {2, 4}, {1, 5}, {2, 6}, {TargetPosX, TargetPosY}] = Path,
    ok.

astar3_test() ->
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, s, 0, 1, 0, t, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    CannotStandPoints = [{1, 5}, {2, 5}, {3, 5}, {4, 5}, {5, 5}, {6, 5}],
    Width = 8,
    Height = 6,
    StartPosX = 3,
    StartPosY = 3,
    TargetPosX = 3,
    TargetPosY = 7,
    MoveSpeed = 1,
    Path = pathFinding:astar(CannotStandPoints, Width, Height, StartPosX, StartPosY, TargetPosX, TargetPosY, MoveSpeed),
    [] = Path,
    ok.

astar4_test() ->
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, 0, 0, 0, 0, 0, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, 0, s, 1, t, 0, 0]
    CannotStandPoints = [{1, 5}, {3, 5}, {4, 5}, {5, 5}, {6, 5}],
    Width = 8,
    Height = 6,
    StartPosX = 6,
    StartPosY = 4,
    TargetPosX = 6,
    TargetPosY = 6,
    MoveSpeed = 1,
    Path = pathFinding:astar(CannotStandPoints, Width, Height, StartPosX, StartPosY, TargetPosX, TargetPosY, MoveSpeed),
    [{6,4},{5,4},{4,4},{3,4},{2,5},{3,6},{4,6},{5,6},{6,6}] = Path,
    ok.

astar5_test() ->
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, 0, 0, 0, 0, 0, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [s, 0, 0, 0, 1, t, 0, 0]
    CannotStandPoints = [{1, 5}, {3, 5}, {4, 5}, {5, 5}, {6, 5}],
    Width = 8,
    Height = 6,
    StartPosX = 6,
    StartPosY = 1,
    TargetPosX = 6,
    TargetPosY = 6,
    MoveSpeed = 1,
    Path = pathFinding:astar(CannotStandPoints, Width, Height, StartPosX, StartPosY, TargetPosX, TargetPosY, MoveSpeed),
    [{6,1},{6,2},{5,3},{4,4},{3,4},{2,5},{3,6},{4,6},{5,6},{6,6}] = Path,
    ok.

astar6_test() ->
    % [0, 0, 0, 0, 0, 0, 0, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, s, 0, 1, 0, t, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, 0, 0, 0, 0, 0, 0]
    % [0, 0, 0, 0, 0, 0, 0, 0]
    CannotStandPoints = [{2, 5}, {3, 5}, {4, 5}],
    Width = 8,
    Height = 6,
    StartPosX = 3,
    StartPosY = 3,
    TargetPosX = 3,
    TargetPosY = 7,
    MoveSpeed = 3,
    Path = pathFinding:astar(CannotStandPoints, Width, Height, StartPosX, StartPosY, TargetPosX, TargetPosY, MoveSpeed),
    [{StartPosX, StartPosY}, {5, 5}, {TargetPosX, TargetPosY}] = Path,
    ok.

astar7_test() ->
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, 0, 0, 0, 0, 0, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [0, 0, 0, 0, 1, 0, 0, 0]
    % [s, 0, 0, 0, 1, t, 0, 0]
    CannotStandPoints = [{1, 5}, {3, 5}, {4, 5}, {5, 5}, {6, 5}],
    Width = 8,
    Height = 6,
    StartPosX = 6,
    StartPosY = 1,
    TargetPosX = 6,
    TargetPosY = 6,
    MoveSpeed = 4,
    Path = pathFinding:astar(CannotStandPoints, Width, Height, StartPosX, StartPosY, TargetPosX, TargetPosY, MoveSpeed),
    [{6,1},{6,2},{4,4},{3,4},{2,5},{3,6},{6,6}] = Path,
    ok.
```