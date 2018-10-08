/*Α.Ε.Μ.:2399
 * Ονοματεπώνυμο:Κωνσταντίνα Παπαδοπούλου

*/

%Facts about sessions, and respective topics

session('Rules; Semantic Technology; and Cross-Industry Standards',
	['XBRL - Extensible Business Reporting Language',
	 'MISMO - Mortgage Industry Standards Maintenance Org',
	 'FIXatdl - FIX Algorithmic Trading Definition Language',
	 'FpML - Financial products Markup Language',
	 'HL7 - Health Level 7',
	 'Acord - Association for Cooperative Operations Research and Development (Insurance Industry)',
	 'Rules for Governance; Risk; and Compliance (GRC); eg; rules for internal audit; SOX compliance; enterprise risk management (ERM); operational risk; etc',
	 'Rules and Corporate Actions']).
session('Rule Transformation and Extraction',
	['Transformation and extraction with rule standards; such as SBVR; RIF and OCL',
	 'Extraction of rules from code',
	 'Transformation and extraction in the context of frameworks such as KDM (Knowledge Discovery meta-model)',
	 'Extraction of rules from natural language',
	 'Transformation or rules from one dialect into another']).
session('Rules and Uncertainty',
	['Languages for the formalization of uncertainty rules',
	 'Probabilistic; fuzzy and other rule frameworks for reasoning with uncertain or incomplete information',
	 'Handling inconsistent or disparate rules using uncertainty',
	 'Uncertainty extensions of event processing rules; business rules; reactive rules; causal rules; derivation rules; association rules; or transformation rules']).
session('Rules and Norms',
	['Methodologies for modeling regulations using both ontologies and rules',
	 'Defeasibility and norms - modeling rule exceptions and priority relations among rules',
	 'The relationship between rules and legal argumentation schemes',
	 'Rule language requirements for the isomorphic modeling of legislation',
	 'Rule based inference mechanism for legal reasoning',
	 'E-contracting and automated negotiations with rule-based declarative strategies']).
session('Rules and Inferencing',
	['From rules to FOL to modal logics',
	 'Rule-based non-monotonic reasoning',
	 'Rule-based reasoning with modalities',
	 'Deontic rule-based reasoning',
	 'Temporal rule-based reasoning',
	 'Priorities handling in rule-based systems',
	 'Defeasible reasoning',
	 'Rule-based reasoning about context and its use in smart environments',
	 'Combination of rules and ontologies',
	 'Modularity']).
session('Rule-based Event Processing and Reaction Rules',
	['Reaction rule languages and engines (production rules; ECA rules; logic event action formalisms; vocabularies/ontologies)',
	 'State management approaches and frameworks',
	 'Concurrency control and scalability',
	 'Event and action definition; detection; consumption; termination; lifecycle management',
	 'Dynamic rule-based workflows and intelligent event processing (rule-based CEP)',
	 'Non-functional requirements; use of annotations; metadata to capture those',
	 'Design time and execution time aspects of rule-based (Semantic) Business Processes Modeling and Management',
	 'Practical and business aspects of rule-based (Semantic) Business Process Management (business scenarios; case studies; use cases etc)']).
session('Rule-Based Distributed/Multi-Agent Systems',
	['rule-based specification and verification of Distributed/Multi-Agent Systems',
	 'rule-based distributed reasoning and problem solving',
	 'rule-based agent architectures',
	 'rules and ontologies for semantic agents',
	 'rule-based interaction protocols for multi-agent systems',
	 'rules for service-oriented computing (discovery; composition; etc)',
	 'rule-based cooperation; coordination and argumentation in multi-agent systems',
	 'rule-based e-contracting and negotiation strategies in multi-agent systems',
	 'rule interchange and reasoning interoperation in heterogeneous Distributed/Multi-Agent Systems']).
session('General Introduction to Rules',
	['Rules and ontologies',
	 'Execution models; rule engines; and environments',
	 'Graphical processing; modeling and rendering of rules']).
session('RuleML-2010 Challenge',
	['benchmarks/evaluations; demos; case studies; use cases; experience reports; best practice solutions (design patterns; reference architectures; models)',
	 'rule-based implementations; tools; applications; demonstrations engineering methods',
	 'implementations of rule standards (RuleML; RIF; SBVR; PRR; rule-based Event Processing languages; BPMN and rules; BPEL and rules); rules and industrial standards (XBRL; MISMO; Accord) and industrial problem statements',
	 'Modelling Rules in the Temporal and Geospatial Applications',
	 'temporal modelling and reasoning; geospatial modelling and reasoning',
	 'cross-linking between temporal and geospatial knowledge',
	 'visualization of rules with graphic models in order to support end-user interaction',
	 'Demos related to various Rules topics',
	 'Extensions and implementations of W3C RIF',
	 'Editing environments and IDEs for Web rules',
	 'Benchmarks and comparison results for rule engines',
	 'Distributed rule bases and rule services',
	 'Reports on industrial experience about rule systems']).

%It counts how many times a string has been found in another string.
count_substring(String, Sub, Total) :-
    count_substring(String, Sub, 0, Total).

count_substring(String, Sub, Count, Total) :-
    ( substring_rest(String, Sub, Rest)
    ->
        succ(Count, NextCount),
        count_substring(Rest, Sub, NextCount, Total)
    ;
        Total = Count
    ).

substring_rest(String, Sub, Rest) :-
    sub_string(String, Before, Length, Remain, Sub),
    DropN is Before + Length,
    sub_string(String, DropN, Remain, 0, Rest).


/*It finds the element "X" , which is placed at the position "Pos",
  in the list "List"
*/
element_at(X, List, Pos) :-
    element_at(X, List, 1, Pos).
element_at(X, [X|_], Pos, Pos).
element_at(X, [_|T], Acc, Pos) :-
    Acc1 is Acc + 1,
    element_at(X, T, Acc1, Pos).



/* It creates the list [H|NewT] which is the same with the list
[H|T] + the element X at the end of the list.
*/
add2end(X,[H|T],[H|NewT]):-add2end(X,T,NewT).
add2end(X,[],[X]).
prepareKeyWords(ListOfK_W,Key,Weight):-
	element_at(Z,ListOfK_W,_),
	term_string(Z,S),%convert the term into string.
	split_string(S, "-", "", L),%split the string rejecting the "-" character and separatingthe keywords and the weights.
	element_at(A,L,1),%It takes the keywords.
	element_at(W,L,2),%It takes the weights.
	tokenize_atom(A,Tokens1),%"tokenize_atom(Atom,List)" converts the string into a list of words.
	tokenize_atom(W,Tokens2),
	element_at(Weight,Tokens2,_),%It finds the weights.
	element_at(Key,Tokens1,_),%It finds keywords.
   %Now we want to reject the "\'" of our elements.
	atom_codes(Key,S1),
	atom_codes('\'',S2),
	length(S1,L1),
	length(S2,L2),
	(L1=:=L2,
	  S1 =\= S2
	; L1=\=L2).

% It finds the score of the themes when there is only one keyword.
scoreThemesOne(X,ListOfK_W,Found):-
	List = [],%I wanted to avoid this line but I didn't manage to do it.
        prepareKeyWords(ListOfK_W,Key,Weight),
	session(X,Y),%We take the ist of the themes.
	element_at(Z,Y,_),
	atom_string(Z,String), %It converts the Atom "Z" into String "String".
	count_substring(String,Key,Count),
	C is Count*Weight,%We calculate the total score of the specific theme.
	add2end(C,List,Found).

%It finds the score of the themes when there are more than one keywords
%scoreThemes(ListOfK_W,last_(Y),Y,[H|T],2,2,2).
scoreThemes(X,ListOfK_W,Found):-
	List=[],%I wanted to avoid this line but I didn't manage to do it.
	prepareKeyWords(ListOfK_W,Key,Weight),
	session(X,Y),%We take the list of the themes.
	element_at(B,Y,_),%We take the themes one-by-one.
	atom_string(B,B_String),%It converts the Atom "B" into String "B_String".
	count_substring(B_String,Key,Count),
	C is Count*Weight,%We calculate the total score of the specific theme.
	add2end(C,List,Found).

scoreTitles(X,ListOfK_W,NewList):-
	List=[],
	prepareKeyWords(ListOfK_W,Key,Weight),
	atom_string(X,String),%It converts the Atom "X" into String "String".
	count_substring(String,Key,Count),
	C is (Count*Weight)*2,%We calculate the total score of the specific theme,
	add2end(C,List,NewList).


scoreSessions(ListOfK_W,NewScList):-
	session(X,_),
	ScoresList=[],
	length(ListOfK_W,L1),
	(L1=:=1,%If there is only one Keyword
	 scoreThemesOne(X,ListOfK_W,ScoreThemes)
	;  L1=\=1,%If there are more than one Keywords
	  scoreThemes(X,ListOfK_W,ScoreThemes)),%Find the score of the themes.
	  scoreTitles(X,ListOfK_W,Temp),%find the score of the tites.
	sum_list(Temp,ScoreTitle),
	append(ScoreThemes,[ScoreTitle],TempScore),%Insert the score
	max_list(TempScore,Max),
	sum_list(TempScore,Sum),
	Scores = 1000 * Max +Sum,
	add2end(Scores,ScoresList,NewScList).

indexOf([Element|_], Element, 0). % We found the element
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1), % Check in the tail of the list
  Index is Index1+1.  % and increment the resulting index

findX([],[]).
findX(XList,NXList):-	   %finds the List of the titles
	session(X,_),
	add2end(X,XList,NXList).


print_Scores(ScoresList):-  %prints the sessions and the relevance of its session.
	max_list(ScoresList,Max),%find the max element of the scores' list.
	indexOf(ScoresList,Max,Index),%find the position of the max element in the list
	findX([],NXList),%find the list of the titles
	element_at(X,NXList,_),% I wanted to insert here the Index(as the position in the list of the titles, where I want to find the title 'X'), but I got a problem so I didn't.As a result the program will not run properly.
	print('Session: '),
	print(X),% print the title
	nl,
	print('Relevance = '),
	print(Max),%print the relevance
	nl,
	delete(ScoresList,Max,NewScList),%delete the previous relevance from the scores' list.
	print_Scores(NewScList).%continue with the next Session

query([]).
query(ListOfK_W):-
	scoreSessions(ListOfK_W,ScoresList),%find the list of the sessions' scores
	print_Scores(ScoresList).%print the sessions and the relevance of its session.
