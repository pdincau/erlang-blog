-module(haversine).
-export([distance/4]).

distance(Lng1, Lat1, Lng2, Lat2) ->
    Deg2rad = fun(Deg) -> math:pi()*Deg/180 end,
    [RLng1, RLat1, RLng2, RLat2] = [Deg2rad(Deg) || Deg <- [Lng1, Lat1, Lng2, Lat2]],

    DLon = RLng2 - RLng1,
    DLat = RLat2 - RLat1,

    A = math:pow(math:sin(DLat/2), 2) + math:cos(RLat1) * math:cos(RLat2) * math:pow(math:sin(DLon/2), 2),

    C = 2 * math:asin(math:sqrt(A)),
    
    %% suppose radius of Earth is 6372.8 km
    Km = 6372.8 * C,
    Km.
