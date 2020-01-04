%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Sistema que entrega el costo total del viaje a 
%%% realizar y te indica opciones adicionales de ciudades.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
iniciar :-
  intro,
  reset_answers,
  hallar_destino(Destino), nl,
  describe_opciones(Destino), nl,
  selecciona, nl.


  intro :-
  write('BIENVENIDO A TU LUGAR IDEAL DE VIAJE'), nl,
  write('A continuacion, se mostraran una serie de preguntas. Para responder SI coloque 0. y para NO coloque 1. (colocar punto seguido luego del numero)'), nl, nl.

  selecciona:-
  write('¿Desea viajar 3 o 4 dias? Para conocer el monto del viaje:'), nl,
  write('Colocar: viaje(lugar,cantidad_dias,Total_Viaje) seguido de un punto'), nl,
  write('Por ejemplo: viaje(iquitos,3,Total_viaje).'), nl.
  
hallar_destino(Destino) :-
 destino(Destino),  !.
   
describe_opciones(Destino):-
 describe(Destino), !.
 
 
% Store user answers to be able to track his progress
:- dynamic(progreso/2).
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%           Reset                    %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
reset_answers :-
  retract(progreso(_, _)),
  fail.
reset_answers.

 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%           Reglas                   %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  destino(iquitos) :-
  temperatura_altas(s_temp),
  playa(n_playa).

  destino(piura) :-
  temperatura_altas(s_temp),
  playa(s_playa). 

  destino(cusco) :-
  temperatura_altas(n_temp),
  museos(n_mus),
  sel_sierra(s_fam),
  ani_bosques(n_bos).
 
  destino(puno) :-
  temperatura_altas(n_temp),
  museos(n_mus),
  sel_sierra(n_fam),
  vivencial(s_viven).
  
  destino(huaraz) :-
  temperatura_altas(n_temp),
  museos(n_mus),
  sel_sierra(n_fam),
  vivencial(n_viven).
 
  destino(huanuco) :-
  temperatura_altas(n_temp),
  museos(n_mus),
  sel_sierra(s_fam),
  ani_bosques(s_bos).
 
  destino(trujillo) :-
  temperatura_altas(n_temp),
  museos(s_mus),
  picante(n_pic).

  destino(arequipa) :-
  temperatura_altas(n_temp),
  museos(s_mus),
  picante(s_pic).
  
  
 
 
  viaje(Lugar,Dias,Total):-vuelo(Lugar,Monto_vuelo), 
                  tour_hotel(Lugar,Dias,Monto_tour), 
                  Total is Monto_vuelo+Monto_tour,
  write('Monto aproximado a gastar en soles (monto del vuelo, tour y alojamiento por persona)').
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%             Preguntas              %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 preguntas(temperatura_altas) :-
  write('¿Te gustan los lugares con climas calidos?'), nl.
 
 preguntas(museos) :-
  write('¿Te gustaria visitar museos, huacas o momias?'), nl.
  
 preguntas(sel_sierra) :-
  write('¿Te interesaria conocer lugares de la sierra y selva?'), nl.
  
 preguntas(playa) :-
  write('¿Te gusta las playas?'), nl.
 
 preguntas(picante) :-
  write('¿Te gusta la comida picante?'), nl.

 preguntas(ani_bosques) :-
  write('¿Te gustaria conocer lagunas y recorrer el bosque?'), nl.
  

 preguntas(vivencial) :-
  write('¿Te gustaria hacer turismo vivencial?'), nl. 
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Base de conocimiento para el costo de viaje %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vuelo(cusco,200).
vuelo(arequipa,250).
vuelo(puno,400).
vuelo(trujillo,240).
vuelo(huaraz,820).
vuelo(piura,290).
vuelo(iquitos,270).
vuelo(huanuco,400).
  
tour_hotel(cusco,3,1000).
tour_hotel(cusco,4,1200).   

tour_hotel(huaraz,3,240). 
tour_hotel(huaraz,4,335).

tour_hotel(puno,3,300).  
tour_hotel(puno,4,490).

tour_hotel(arequipa,3,600). 
tour_hotel(arequipa,4,780).

tour_hotel(iquitos,3,600). 
tour_hotel(iquitos,4,850).

tour_hotel(huanuco,3,380).
tour_hotel(huanuco,4,430).

tour_hotel(piura,3,690). 
tour_hotel(piura,4,750).

tour_hotel(trujillo,3,280). 
tour_hotel(trujillo,4,450).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%      Asignar respuestas            %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 temperatura_altas(Respuesta) :-
  progreso(temperatura_altas, Respuesta).
  
 temperatura_altas(Respuesta) :-
  \+ progreso(temperatura_altas, _),
  ask(temperatura_altas, Respuesta, [s_temp, n_temp]).
 
 museos(Respuesta) :-
  progreso(museos, Respuesta).
 museos(Respuesta) :-
  \+ progreso(museos, _),
  ask(museos, Respuesta, [s_mus, n_mus]). 
  
  sel_sierra(Respuesta) :-
  progreso(sel_sierra, Respuesta).
 sel_sierra(Respuesta) :-
  \+ progreso(sel_sierra, _),
  ask(sel_sierra, Respuesta, [s_fam, n_fam]).

  playa(Respuesta) :-
  progreso(playa, Respuesta).
 playa(Respuesta) :-
  \+ progreso(playa, _),
  ask(playa, Respuesta, [s_playa, n_playa]).
  
  picante(Respuesta) :-
  progreso(picante, Respuesta).
 picante(Respuesta) :-
  \+ progreso(picante, _),
  ask(picante, Respuesta, [s_pic, n_pic]).
  
  ani_bosques(Respuesta) :-
  progreso(ani_bosques, Respuesta).
 ani_bosques(Respuesta) :-
  \+ progreso(ani_bosques, _),
  ask(ani_bosques, Respuesta, [s_bos, n_bos]).
  
  vivencial(Respuesta) :-
  progreso(vivencial, Respuesta).
 vivencial(Respuesta) :-
  \+ progreso(vivencial, _),
  ask(vivencial, Respuesta, [s_viven, n_viven]).
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    Lista de alternativas de las preguntas %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

respuesta(s_temp) :-
  write('Si me encanta, un clima tropical').

respuesta(n_temp) :-
  write('No, me gustan las temperaturas altas'), nl.

respuesta(n_mus) :-
  write('No, disfruto caminar al aire libre'), nl.

respuesta(s_mus) :-
  write('Si, prefiero conocer museos, centros historicos').
  
respuesta(s_fam) :-
  write('Si, me encantaria conocer esos lugares').

respuesta(n_fam) :-
  write('No, me afectan los cambios de climas'), nl.
 
 respuesta(s_playa) :-
  write('Si, me encanta la playa').

respuesta(n_playa) :-
  write('No disfruto de la playa'), nl.


respuesta(s_pic) :-
  write('Si, me encanta la comida picante como el rocoto relleno').

respuesta(n_pic) :-
  write('No, prefiero la comida sin nada de picante'), nl.

respuesta(s_bos) :-
  write('Si, quiero conocer los paisajes naturales de Peru').

respuesta(n_bos) :-
  write('No, no me gustan los bosques'), nl.

respuesta(s_viven) :-
  write('Si, quiero eperimentar esa experiencia').

respuesta(n_viven) :-
  write('No,quiero hacer recorridos largos y conocer paisajes escondidos'), nl.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%  Mostrar lista de alternativas        %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % [First|Rest] is the Choices list, Index is the index of First in Choices 
  alternativas([], _).
  alternativas([First|Rest], Index) :-
  write(Index), write(' '), respuesta(First), nl,
  NextIndex is Index + 1,
  alternativas(Rest, NextIndex).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%      Texto para dar la Respuestas        %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
describe(iquitos) :- 
write('Una buena opcion para que viajes es Iquitos'), nl.

describe(piura) :- 
  write('Una buena opcion para que viajes es Piura'), nl.
 
describe(cusco) :- 
  write('Una buena opcion para que viajes es Cusco'), nl.

describe(puno) :-  
  write('Una buena opcion para que viajes es Puno'), nl. 
  
  describe(huaraz) :- 
  write('Una buena opcion para que viajes es Huaraz'), nl.
  
  describe(huanuco) :- 
  write('Una buena opcion para que viajes es Huanuco'), nl.

  describe(trujillo) :- 
  write('Una buena opcion para que viajes es Trujillo'), nl.
  
  describe(arequipa) :- 
  write('Una buena opcion para que viajes es Arequipa'), nl.
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Parses an Index and returns a Response representing the "Indexth" element in
%  Choices (the [First|Rest] list)
 
  parse(0, [First|_], First).
  parse(Index, [First|Rest], Response) :-
  Index > 0,
  NextIndex is Index - 1,
  parse(NextIndex, Rest, Response).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% Leer y guardar la alternativa seleccionada   %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	  
% Asks the Question to the user and saves the Answer

  ask(Preguntas, Respuesta, Choices) :-
  preguntas(Preguntas),
  alternativas(Choices, 0),
  read(Index),
  parse(Index, Choices, Response),
  asserta(progreso(Preguntas, Response)),
  Response = Respuesta.
  

  




