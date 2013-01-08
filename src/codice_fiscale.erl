-module(codice_fiscale).

-export([calculate/8, calculate/9, alternative/1,
    calculate_alternative/9, calculate_alternative/10]).

surname(Surname = [_|_]) -> surname(Surname, [], []).

name(Name = [_|_]) -> name(Name, [], [], nil).

bday_gender(Gender, Day, Month, Year) ->
  string:right(i2s(Year), 2, $0) ++
    month(Month) ++
    string:right(i2s(day(Gender, Day)), 2, $0).

belfiore(Province = [_,_], City = [_|_]) ->
  belfiore:get(string:to_upper(City), string:to_upper(Province)).

crc(Name = [_,_,_], Surname = [_,_,_], GenderDate = [_,_,_,_,_], Belfiore = [_,_,_,_]) ->
  [(crc(Surname, 0) +
    crc(Name, 3) +
    crc(GenderDate, 6) +
    crc(Belfiore, 11)) rem 26 + 65].

add_crc([Name, Surname, GenderDate, Belfiore]) ->
  add_crc(Name, Surname, GenderDate, Belfiore).
add_crc([Name, Surname, GenderDate, Belfiore], Sep) ->
  add_crc(Name, Surname, GenderDate, Belfiore, Sep).
add_crc(Name, Surname, GenderData, Belfiore, Sep) ->
  Surname ++ Sep ++
    Name ++ Sep ++
    GenderData ++ Sep ++
    Belfiore ++ Sep ++
    crc(Name, Surname, GenderData, Belfiore).
add_crc(Name, Surname, GenderData, Belfiore) ->
  Surname ++ Name ++ GenderData ++ Belfiore ++
    crc(Name, Surname, GenderData, Belfiore).

calculate(Name, Surname, Gender, Day, Month, Year, Province, City, Separator) ->
  add_crc(name(string:to_upper(Name)),
    surname(string:to_upper(Surname)),
    bday_gender(Gender, Day, Month, Year),
    belfiore(Province, City), Separator).
calculate(Name, Surname, Gender, Day, Month, Year, Province, City) ->
  add_crc(name(string:to_upper(Name)),
    surname(string:to_upper(Surname)),
    bday_gender(Gender, Day, Month, Year),
    belfiore(Province, City)).

calculate_alternative(Level, Name, Surname, Gender, Day, Month, Year, Province, City) ->
  add_crc(encode_alternative(Level, name(string:to_upper(Name)),
        surname(string:to_upper(Surname)),
      bday_gender(Gender, Day, Month, Year),
      belfiore(Province, City))).
calculate_alternative(Level, Name, Surname, Gender, Day, Month, Year, Province, City, Separator) ->
  add_crc(encode_alternative(Level, name(string:to_upper(Name)),
        surname(string:to_upper(Surname)),
      bday_gender(Gender, Day, Month, Year),
      belfiore(Province, City)), Separator).

alternative($0) -> $L;
alternative($1) -> $M;
alternative($2) -> $N;
alternative($3) -> $P;
alternative($4) -> $Q;
alternative($5) -> $R;
alternative($6) -> $S;
alternative($7) -> $T;
alternative($8) -> $U;
alternative($9) -> $V.



belfiore_alternative(Level, [B1, B2, B3, B4]) when Level >= 3 ->
  [B1, alternative(B2), alternative(B3), alternative(B4)];
belfiore_alternative(2, [B1, B2, B3, B4]) ->
  [B1, B2, alternative(B3), alternative(B4)];
belfiore_alternative(1, [B1, B2, B3, B4]) ->
  [B1, B2, B3, alternative(B4)];
belfiore_alternative(_, Belfiore) -> Belfiore.


genderdate_alternative(Level, [G1, G2, G3, G4, G5]) when Level >= 4 ->
  [alternative(G1), alternative(G2), G3, alternative(G4), alternative(G5)];
genderdate_alternative(3, [G1, G2, G3, G4, G5]) ->
  [G1, alternative(G2), G3, alternative(G4), alternative(G5)];
genderdate_alternative(2, [G1, G2, G3, G4, G5]) ->
  [G1, G2, G3, alternative(G4), alternative(G5)];
genderdate_alternative(1, [G1, G2, G3, G4, G5]) ->
  [G1, G2, G3, G4, alternative(G5)];
genderdate_alternative(_, GenderDate) -> GenderDate.


encode_alternative(Level, N, S, GenderDate, Belfiore) ->
  [N, S, genderdate_alternative(Level - 3, GenderDate), belfiore_alternative(Level, Belfiore)].


pad3(List) -> string:left(List, 3, $X).


surname(_, Cs = [_,_,_], _) -> lists:reverse(Cs);

surname([], Cs, Vs) ->
  pad3(lists:reverse(Vs ++ Cs));

surname([H|T], Cs, Vs) when H =:= $A orelse
  H =:= $E orelse
  H =:= $I orelse
  H =:= $O orelse
  H =:= $U ->
  if
    length(Vs) < 3 -> surname(T, Cs, [H|Vs]);
    true -> surname(T, Cs, Vs)
  end;

surname([H|T], Cs, Vs) ->
  surname(T, [H|Cs], Vs).


name(_, Cs = [_,_,_], _, _) -> lists:reverse(Cs);

name([], Cs, Vs, nil) -> surname([], Cs, Vs);

name([], [C1,C2], Vs, C) -> surname([], [C1, C, C2], Vs);

name([H|T], Cs, Vs, C) when H =:= $A orelse
  H =:= $E orelse
  H =:= $I orelse
  H =:= $O orelse
  H =:= $U ->
  if
    length(Vs) < 3 -> name(T, Cs, [H|Vs], C);
    true -> name(T, Cs, Vs, C)
  end;

name([H|T], Cs = [_], Vs, nil) -> name(T, Cs, Vs, H);

name([H|T], Cs, Vs, C) -> name(T, [H|Cs], Vs, C).


i2s(I) -> hd(io_lib:format("~p", [I])).


month(1)  -> "A";
month(2)  -> "B";
month(3)  -> "C";
month(4)  -> "D";
month(5)  -> "E";
month(6)  -> "H";
month(7)  -> "L";
month(8)  -> "M";
month(9)  -> "P";
month(10) -> "R";
month(11) -> "S";
month(12) -> "T".


day(m, D) -> D;
day(f, D) -> D + 40.

a2c(0, $0) -> 1;
a2c(0, $1) -> 0;
a2c(0, $2) -> 5;
a2c(0, $3) -> 7;
a2c(0, $4) -> 9;
a2c(0, $5) -> 13;
a2c(0, $6) -> 15;
a2c(0, $7) -> 17;
a2c(0, $8) -> 19;
a2c(0, $9) -> 21;
a2c(0, $A) -> 1;
a2c(0, $B) -> 0;
a2c(0, $C) -> 5;
a2c(0, $D) -> 7;
a2c(0, $E) -> 9;
a2c(0, $F) -> 13;
a2c(0, $G) -> 15;
a2c(0, $H) -> 17;
a2c(0, $I) -> 19;
a2c(0, $J) -> 21;
a2c(0, $K) -> 2;
a2c(0, $L) -> 4;
a2c(0, $M) -> 18;
a2c(0, $N) -> 20;
a2c(0, $O) -> 11;
a2c(0, $P) -> 3;
a2c(0, $Q) -> 6;
a2c(0, $R) -> 8;
a2c(0, $S) -> 12;
a2c(0, $T) -> 14;
a2c(0, $U) -> 16;
a2c(0, $V) -> 10;
a2c(0, $W) -> 22;
a2c(0, $X) -> 25;
a2c(0, $Y) -> 24;
a2c(0, $Z) -> 23;

a2c(1, $0) -> 0;
a2c(1, $1) -> 1;
a2c(1, $2) -> 2;
a2c(1, $3) -> 3;
a2c(1, $4) -> 4;
a2c(1, $5) -> 5;
a2c(1, $6) -> 6;
a2c(1, $7) -> 7;
a2c(1, $8) -> 8;
a2c(1, $9) -> 9;
a2c(1, $A) -> 0;
a2c(1, $B) -> 1;
a2c(1, $C) -> 2;
a2c(1, $D) -> 3;
a2c(1, $E) -> 4;
a2c(1, $F) -> 5;
a2c(1, $G) -> 6;
a2c(1, $H) -> 7;
a2c(1, $I) -> 8;
a2c(1, $J) -> 9;
a2c(1, $K) -> 10;
a2c(1, $L) -> 11;
a2c(1, $M) -> 12;
a2c(1, $N) -> 13;
a2c(1, $O) -> 14;
a2c(1, $P) -> 15;
a2c(1, $Q) -> 16;
a2c(1, $R) -> 17;
a2c(1, $S) -> 18;
a2c(1, $T) -> 19;
a2c(1, $U) -> 20;
a2c(1, $V) -> 21;
a2c(1, $W) -> 22;
a2c(1, $X) -> 23;
a2c(1, $Y) -> 24;
a2c(1, $Z) -> 25.

crc([H|T], N) ->
  crc(T, N + 1, a2c(N rem 2, H)).
crc([H|T], N, R) ->
  crc(T, N + 1, a2c(N rem 2, H) + R);
crc([], _, R) -> R.
