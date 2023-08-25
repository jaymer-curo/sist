:- use_module(library(jpl)).
start:-sleep(0.4),
		write('-----------------------------------------------------------------'),nl,
		sleep(0.4),
		write('*****************************************************************'),nl,
		sleep(0.2),
		write("###################||| SISTEMA EXPERTO |||#########################"),nl,
		sleep(0.4),
		write('*****************************************************************'),nl,
		sleep(0.4),
		write('-----------------------------------------------------------------'),nl,nl,nl,
        /*write("Hola.  Como estas? Primero que nada dime tu nombre por favor : "),
        read(Paciente),*/


		interface2.


       /* hipotesis(Paciente,Enfermedad),
        write(Paciente),write(', tu '), write(' probablemente tenga '),write(Enfermedad),write('.'),undo,
		nl,nl,nl,
		sleep(0.7),
		write('*****************************************************************'),nl,
		sleep(0.4),
		write("################||| THANK YOU FOR USE ME |||#####################"),nl,
		sleep(0.4),
		write('*****************************************************************'),nl.*/


    simtoma(Paciente,ideas_delirantes ) :- verificar(Paciente," tiene ideas delirantes   (si/no) ?").

    simtoma(Paciente,alucinaciones) :- verificar(Paciente,"tiene alucinaciones (si/no) ?").

    simtoma(Paciente,ansiedad) :- verificar(Paciente," tiene ansiedad (si/no) ?").

    simtoma(Paciente,ira) :- verificar(Paciente," tiene ira (si/no) ?").

    simtoma(Paciente,lenguaje_desorganizado) :- verificar(Paciente," tienes lenguaje desorganizado (si/no) ?").

    simtoma(Paciente,comportamiento_desorganizado) :- verificar(Paciente," tiene comportamiento desorganizado (si/no) ?").

    simtoma(Paciente,alteraciones_emocionales) :- verificar(Paciente," tiene alteraciones emocionales (si/no) ?").

    simtoma(Paciente,inmovilidad) :- verificar(Paciente," tiene inmovilidad (si/no) ?").

    simtoma(Paciente,actividad_motora_excesiva) :- verificar(Paciente," tiene un actividad motora excesiva (si/no) ?").

    simtoma(Paciente,mutismo) :- verificar(Paciente," tiene mutismo (si/no) ?").

    simtoma(Paciente,embotamiento_afectivo) :- verificar(Paciente," tiene embotamiento afectivo (si/no) ?").

    simtoma(Paciente,aislamiento_emocional) :- verificar(Paciente," tiene aislamiento emocional (si/no) ?").

     simtoma(Paciente,perdida_de_interes_en_una_actividad) :- verificar(Paciente," tiene perdida de interes en una actividad  (si/no) ?").

	/*simtoma(_,"Lo siento, parece que no puedo diagnosticar la Enfermedad.").*/


    hipotesis(Paciente,esquizofrenia_paranoide) :-
        simtoma(Paciente,ideas_delirantes),
        simtoma(Paciente,alucinaciones),
        simtoma(Paciente,ansiedad),
        simtoma(Paciente,ira).


    hipotesis(Paciente,esquizofrenia_desorganizado) :-
        simtoma(Paciente,lenguaje_desorganizado),
        simtoma(Paciente,comportamiento_desorganizado),
        simtoma(Paciente,alteraciones_emocionales),
        simtoma(Paciente,aislamiento_emocional).

    hipotesis(Paciente,esquizofrenia_catatonico) :-
        simtoma(Paciente,inmovilidad),
        simtoma(Paciente,actividad_motora_excesiva),
        simtoma(Paciente,mutismo),
        simtoma(Paciente,embotamiento_afectivo).

     hipotesis(Paciente,esquizofrenia_simple) :-
        simtoma(Paciente,ideas_delirantes),
        simtoma(Paciente,actividad_motora_excesiva),
        simtoma(Paciente,alucinaciones),
        simtoma(Paciente,embotamiento_afectivo).

    hipotesis(Paciente,esquizofrenia_residual) :-
        simtoma(Paciente,aislamiento_emocional),
        simtoma(Paciente,lenguaje_desorganizado),
        simtoma(Paciente,perdida_de_interes_en_una_actividad),
        simtoma(Paciente,embotamiento_afectivo).



	hipotesis(_,"enfermedad. Pero lo siento, parece que no puedo diagnosticar la dicha enfermedad").

    response(Reply) :-
        read(Reply),
        write(Reply),nl.

preguntar(Paciente,Pregunta) :-
	interface(', usted',Paciente,Pregunta),
	write('Loading.'),nl,
	sleep(1),
	write('Loading..'),nl,
	sleep(1),
	write('Loading...'),nl,
	sleep(1),
    nl.

:- dynamic si/1,no/1.

verificar(P,S) :-
   (si(S)
    ->
    true ;
    (si(S)
     ->
     fail ;
     preguntar(P,S))).

undo :- retract(si(_)),fail.
undo :- retract(no(_)),fail.
undo.


pt(Paciente):-
                hipotesis(Paciente,Enfermedad),
		interface3(Paciente,', tu probablemente tengas ',Enfermedad,'.'),
        write(Paciente),write(', tu probablemente tengas '),write(Enfermedad),write('.'),undo,end.


end :-
		nl,nl,nl,
		sleep(0.7),
		write('*****************************************************************'),nl,
		sleep(0.4),
		write("################||| THANK YOU FOR USE ME |||#####################"),nl,
		sleep(0.4),
		write('*****************************************************************'),nl.

interface(X,Y,Z) :-
	atom_concat(Y,X, FAtom),
	atom_concat(FAtom,Z,FinalAtom),
	jpl_new('javax.swing.JFrame', ['Esquizofrenia'], F),
	jpl_new('javax.swing.JLabel',['---Sistema test de esquizofrenia ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [400,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showInputDialog, [F,FinalAtom], N),
	jpl_call(F, dispose, [], _),
	write(N),nl,
	( (N == si ; N == s)
      ->
       assert(si(Z)) ;
       assert(no(Z)), fail).

interface2 :-
	jpl_new('javax.swing.JFrame', ['Esquizofrenia'], F),
	jpl_new('javax.swing.JLabel',['--- Sistema test de esquizofrenia ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [400,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showInputDialog, [F,'Hola.  Como estas? Primero que nada dime tu nombre por favor'], N),
	jpl_call(F, dispose, [], _),
	/*write(N),nl,*/
	(	N == @(null)
		->	write('cancelaste'),interface3('cancelaste. ','gracias ','por ','usarme.'),end,fail
		;	write("Hola.  Como estas? Primero que nada dime tu nombre por favor : "),write(N),nl,pt(N)
	).


interface3(P,W1,D,W2) :-
	atom_concat(P,W1, A),
	atom_concat(A,D,B),
	atom_concat(B,W2,W3),
	jpl_new('javax.swing.JFrame', ['Esquizofrenia'], F),
	jpl_new('javax.swing.JLabel',['--- Sistema test de esquizofrenia ---'],LBL),
	jpl_new('javax.swing.JPanel',[],Pan),
	jpl_call(Pan,add,[LBL],_),
	jpl_call(F,add,[Pan],_),
	jpl_call(F, setLocation, [400,300], _),
	jpl_call(F, setSize, [400,300], _),
	jpl_call(F, setVisible, [@(true)], _),
	jpl_call(F, toFront, [], _),
	jpl_call('javax.swing.JOptionPane', showMessageDialog, [F,W3], N),
	jpl_call(F, dispose, [], _),
	/*write(N),nl,*/
	(	N == @(void)
		->	write('')
		;	write("")
	).

help :- write("To start the expert system please type 'start.' and press Enter key").
