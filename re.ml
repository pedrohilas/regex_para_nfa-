open Ast

type estado = int 
type simbolo = char option
type transicao = (estado * simbolo * estado)
type automato = {
  estados: estado list;
  iniciais: estado list;
  finais: estado list;
  transicoes: transicao list
}


let novo_estado =
  let contador = ref 0 in
  fun () -> incr contador; !contador

let rec regex_para_nfa regex =
  match regex with
  | Vazio -> 
    let estado = novo_estado () in
    { estados=[estado]; iniciais=[estado]; finais=[estado]; transicoes=[] }

    | Caracter c -> 
      let inicio = novo_estado () in
      let final = novo_estado () in
      {
        estados = [inicio; final];
        iniciais = [inicio];
        finais = [final];
        transicoes = [
          (inicio, Some c, final)  
        ]
      }

      | Concatenacao (a, b) ->
        let nfa_b = regex_para_nfa b in  
        let nfa_a = regex_para_nfa a in  
        let transicoes_epsilon = List.map (fun f -> (f, None, List.hd nfa_b.iniciais)) nfa_a.finais in
        {
          estados = nfa_a.estados @ nfa_b.estados;
          iniciais = nfa_a.iniciais;
          finais = nfa_b.finais;
          transicoes = transicoes_epsilon @ nfa_a.transicoes @ nfa_b.transicoes
        }
      

        | Escolha (a, b) ->
          
          let nfa_b = regex_para_nfa b in  
          let nfa_a = regex_para_nfa a in  
          let novo_inicio = novo_estado () in
          {
            estados = novo_inicio :: (nfa_b.estados @ nfa_a.estados); 
            iniciais = [novo_inicio];
            finais = nfa_b.finais @ nfa_a.finais; 
            transicoes =
              (List.map (fun ini -> (novo_inicio, None, ini)) ( nfa_a.iniciais @ nfa_b.iniciais)) @  (* Transições epsilon do novo início para os dois ramos *)
              nfa_a.transicoes @ nfa_b.transicoes
          }
        
          | Estrela a ->
            let nfa = regex_para_nfa a in
            let novo_inicio_final = novo_estado () in
            {
              estados = novo_inicio_final :: nfa.estados;
              iniciais = [novo_inicio_final];
              finais = novo_inicio_final :: nfa.finais;  
              transicoes = 
                [(novo_inicio_final, None, List.hd nfa.iniciais)] @  
                (List.map (fun f -> (f, None, List.hd nfa.iniciais)) nfa.finais) @  
                nfa.transicoes
            }
        

            let imprimir_automato automato =
              let espacos = "    "  (* Quatro espaços *)
              in
              (* Imprime o número total de estados *)
              Printf.printf "%s%d\n" espacos (List.length automato.estados);
              
              (* Imprime o número de estados iniciais *)
              Printf.printf "%s%d\n" espacos 1;
              
              (* Imprime o estado inicial *)
              Printf.printf "%s%d\n" espacos (List.hd automato.iniciais);
              
              (* Ordena os estados finais de forma inversa *)
              let finais_ordenados_inverso = List.sort (fun a b -> compare b a) automato.finais in
              
              (* Imprime o número de estados finais *)
              Printf.printf "%s%d\n" espacos (List.length finais_ordenados_inverso);
              
              (* Imprime os estados finais em ordem inversa *)
              Printf.printf "%s%s\n" espacos (String.concat " " (List.map string_of_int finais_ordenados_inverso));
              
              (* Imprime o número de transições *)
              Printf.printf "%s%d\n" espacos (List.length automato.transicoes);
              
              (* Imprime as transições na ordem em que foram adicionadas *)
              let rec imprimir_transicoes = function
                | [] -> ()
                | [t] -> (* Última transição sem nova linha *)
                    let (i, s, j) = t in
                    Printf.printf "%s%d %s %d" espacos i (match s with Some c -> String.make 1 c | None -> "_") j
                | t :: ts ->
                    let (i, s, j) = t in
                    Printf.printf "%s%d %s %d\n" espacos i (match s with Some c -> String.make 1 c | None -> "_") j;
                    imprimir_transicoes ts
              in
              imprimir_transicoes automato.transicoes;
              flush stdout  (* Força o esvaziamento do buffer sem adicionar nova linha *)
            
            let () =
              let expr_string = read_line () in
              let regex = Parser.main Lexer.token (Lexing.from_string expr_string) in
              let automato = regex_para_nfa regex in
              imprimir_automato automato  