-module(haversine).
-export([distance/4]).

distance(Lng1, Lat1, Lng2, Lat2) ->
    ToRadians = fun(Deg) -> 2*math:pi()*Deg/360 end,
    [RLng1, RLat1, RLng2, RLat2] = [ToRadians(Deg) || Deg <- [Lng1, Lat1, Lng2, Lat2]],

    DLon = RLng2 - RLng1,
    DLat = RLat2 - RLat1,

    A = math:pow(math:sin(DLat/2), 2) + math:cos(Lat1) * math:cos(Lat2) * math:pow(math:sin(DLon/2), 2),

    C = 2 * math:asin(math:sqrt(A)),
    
    %% consider Earth radius equal to 6371km
    Km = 6371 * C,
    Km.
