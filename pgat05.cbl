      ***************************************************************
      *                                                             *
      *  A B A V / C N - I N F O R M A T I C A    :::  PGAT05       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Atualizacao do Cadastro :                                  *
      *                                                             *
      *  Data da ultima alteracao:    07/11/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgat05.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
      *
           select arqat01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is at01-chave
                  alternate record key is at01-chave-1 with duplicates
                  alternate record key is at01-chave-2 with duplicates
                  file status is at01-status.
      *
           select arqass assign to disk
                  organization is line sequential
                  access mode is sequential
                  lock mode is manual
                  file status is ass-status.
      *
       data division.
       file section.
      *    
       copy fdat01.lib.
      *
       copy fdass.lib.
      *
       working-storage section.
      *
       01 at01-status                  pic x(02) value "00".
       01 at01-stat                    pic x(01) value "F".
      *
       01 nome-arq-at01.
          02 at01-dir                  pic x(03) value "AT2".
          02 filler                    pic x(01) value "\".
          02 at01-nome                 pic x(08) value "ARQAT01A".
          02 filler                    pic x(01) value ".".
          02 at01-ext                  pic x(03) value "DAT".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGAT05".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 impress                      pic x(12) value spaces.
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 ass-status                   pic x(02) value "00".
       01 ass-stat                     pic x(01) value "F".
      *
       01 nome-arq-ass.
          02 ass-dir                   pic x(03) value "TMP".
          02 filler                    pic x(01) value "\".
          02 ass-nome                  pic x(08) value "ARQASSOC".
          02 filler                    pic x(01) value ".".
          02 ass-ext                   pic x(03) value "DAT".
      *
       01 limpa                        pic x(15) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 spool                        pic x(04) value spaces.
       01 campo-wait-aux               pic 9(04) comp-5 value 2.
       01 linha                        pic 9(03) comp-5 value 0.
       01 coluna                       pic 9(04) comp-5 value 1.
       01 tamanho                      pic 9(04) comp-5 value 78.
       01 pagina                       pic 9(03) value 0.
       01 tracos-i                     pic x(80) value all "-".
       01 sequencia                    pic 9(05) value 0.
       01 sub                          pic 9(03) value 0.
       01 sub1                         pic 9(03) value 0.
       01 tot-lidos                    pic 9(04) value 0.
       01 tot-impressos                pic 9(04) value 0.
      *
       01 campos.
          02 codigo                    pic x(06) value spaces.
          02 empresa                   pic x(40) value spaces.
          02 empresa-a                 pic x(40) value spaces.
          02 diretor                   pic x(40) value spaces.
          02 diretor-a                 pic x(40) value spaces.
          02 endereco                  pic x(40) value spaces.
          02 cep                       pic 9(08) value 0.
          02 cidade                    pic x(15) value spaces.
          02 uf                        pic x(02) value spaces.
          02 ddd                       pic 9(04) value 0.
          02 telefone                  pic x(08) value spaces.
          02 telex                     pic x(08) value spaces.
          02 fax                       pic x(08) value spaces.
      *
       01 tabela.
          02 tab-reg                   pic x(01) occurs 250.
      *
       01 tabela-aux.
          02 tab-reg-aux               pic x(01) occurs 55.
      *
       01 data-aux.
          02 dia-aux                   pic 9(02) value 0.
          02 mes-aux                   pic 9(02) value 0.
          02 ano-aux                   pic 9(02) value 0.
      *
       copy workgen.lib.
      * 
       linkage section.
      *
       01 param-menu.
          02 param-usr                 pic x(10).
          02 param-senha               pic x(10).
          02 param-prioridade          pic 9(01).
          02 param-data                pic 9(05).
          02 param-impress             pic x(12).
      *
       screen section.
      *
       01 tela-mensagem-cad.
          02 line 20 column 05 foreground-color 07 background-color 02
             highlight pic x(41) from mensagem.
      *
       01 tela-erro-cad.
          02 line 20 column 05 beep reverse-video pic x(41) from 
             mensagem.
      *
       01 tela-limpa-cad.
          02 line 20 column 05 foreground-color 04 background-color 04
             pic x(41) from spaces.
      *
       copy scrgen.lib.
      *
       procedure division using param-menu.
      *
       lab-00.
           perform sec-atualizacao.
           perform sec-deleta.
      *
       lab-fim.
           exit program.
      *
      ************************
      *                      *
      *    R o t i n a s     *
      *                      *
      ************************
      *      
       rotinas section.
      *
       rot-move-at01.
      *     move codigo to at01-codigo.
           move empresa to at01-empresa.
           move empresa-a to at01-empresa-a.
           move diretor to at01-diretor.
           move diretor-a to at01-diretor-a.
           move endereco to at01-endereco.
           move uf to at01-uf.
           move cidade to at01-cidade.
           move cep to at01-cep.
           move telefone to at01-telefone.
           move ddd to at01-ddd.
           move fax to at01-fax.
           move telex to at01-telex.
           move param-usr to at01-usuario.
           move param-data to at01-data.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqass at end move 1 to erro.
           if ass-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-at01.
           move 0 to erro.
           if at01-stat = "F"
              open i-o arqat01
              if at01-status not = "00"
                 move 
                 " Erro de abertura no ARQAT01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to at01-stat
               end-if
           end-if.
      *
       rot-close-at01.
           if at01-stat = "A"
              close arqat01
              move "F" to at01-stat
           end-if.
      *
       rot-le-at01.
           move 0 to erro.
           read arqat01 invalid key move 1 to erro.
           if at01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-at01
           end-if.
      *
       rot-le-proximo-at01.
           move 0 to erro.
           read arqat01 next at end move 1 to erro.
           if at01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo-at01
           end-if.
      *
       err-leitura-at01.
           move " Erro de leitura - ARQAT01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-open-ass.
           move 0 to erro.
           move zeros to ass-status
           if ass-stat = "F"
              open input arqass
              if ass-status not = "00"
                 move " Erro de abertura - Tecle <Enter>" to mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
              else
                 move "A" to ass-stat
              end-if
           end-if.
      *
       rot-close-ass.
           if ass-stat = "A"
              close arqass 
              unlock arqass
              move "F" to ass-stat
           end-if.
      *
       rot-erro-leitura-at01.
           move " Erro de leitura - ARQAT01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-desmembra.
           move 1 to sub.
           move 1 to sub1.
           move spaces to tabela-aux.
           perform until tab-reg (sub)     = """" and 
                         tab-reg (sub + 1) = "," and
                         tab-reg (sub + 2) = """"
                         move tab-reg (sub) to tab-reg-aux (sub1)
                         add 1 to sub sub1
           end-perform.
           move tabela-aux to empresa-a txt.
           perform rot-texto.
           move txt to empresa.
           add 3 to sub.
           move 1 to sub1.
           move spaces to tabela-aux.
           perform until tab-reg (sub)     = """" and 
                         tab-reg (sub + 1) = "," and
                         tab-reg (sub + 2) = """"
                         move tab-reg (sub) to tab-reg-aux (sub1)
                         add 1 to sub sub1
           end-perform.  
           move tabela-aux to endereco.
           add 3 to sub.
           move 1 to sub1.
           move spaces to tabela-aux.
           perform until tab-reg (sub)     = """"  and 
                         tab-reg (sub + 1) = "," and
                         tab-reg (sub + 2) = """"
                         move tab-reg (sub) to tab-reg-aux (sub1)
                         add 1 to sub sub1
           end-perform.  
           move tabela-aux to cidade.
           add 3 to sub.
           move 1 to sub1.
           move spaces to tabela-aux.
           perform until tab-reg (sub)     = """"  and 
                         tab-reg (sub + 1) = ","
                         move tab-reg (sub) to tab-reg-aux (sub1)
                         add 1 to sub sub1
           end-perform.
           move tabela-aux to uf.
           add 2 to sub.
           move 1 to sub1.
           move spaces to tabela-aux.
           perform until tab-reg (sub) = ","  
                         move tab-reg (sub) to tab-reg-aux (sub1)
                         add 1 to sub sub1
           end-perform.
           if tab-reg-aux (08) = spaces
              move tab-reg-aux (07) to tab-reg-aux (08)
              move tab-reg-aux (06) to tab-reg-aux (07)
              move tab-reg-aux (05) to tab-reg-aux (06)
              move tab-reg-aux (04) to tab-reg-aux (05)
              move tab-reg-aux (03) to tab-reg-aux (04)
              move tab-reg-aux (02) to tab-reg-aux (03)
              move tab-reg-aux (01) to tab-reg-aux (02)
              move 0 to tab-reg-aux (01)
           end-if.
           move tabela-aux to cep.
           add 2 to sub.
           move 1 to sub1.
           move spaces to tabela-aux.
           perform until tab-reg (sub) = """"   and 
                         tab-reg (sub + 1) = ","
                         move tab-reg (sub) to tab-reg-aux (sub1)
                         add 1 to sub sub1
           end-perform.  
           move tabela-aux to diretor-a txt.
           perform rot-texto.
           move txt to diretor.
           add 2 to sub.
           move 1 to sub1.
           move spaces to tabela-aux.
           perform until tab-reg (sub) = ","
                         move tab-reg (sub) to tab-reg-aux (sub1)
                         add 1 to sub sub1
           end-perform.  
           if tab-reg-aux (03) = spaces
              move tab-reg-aux (02) to tab-reg-aux (04)
              move tab-reg-aux (01) to tab-reg-aux (03)
              move 0 to tab-reg-aux (01) tab-reg-aux (02)
           end-if.
           if tab-reg-aux (04) = spaces
              move tab-reg-aux (03) to tab-reg-aux (04)
              move tab-reg-aux (02) to tab-reg-aux (03)
              move tab-reg-aux (01) to tab-reg-aux (02)
              move 0 to tab-reg-aux (01)
           end-if.
           move tabela-aux to ddd.
           add 2 to sub.
           move 1 to sub1.
           move spaces to tabela-aux.
           perform until tab-reg (sub) = """" and 
                         tab-reg (sub + 1) = ","
                         move tab-reg (sub) to tab-reg-aux (sub1)
                         add 1 to sub sub1
           end-perform.  
           move tabela-aux to telefone.
           add 3 to sub.
           move 1 to sub1.
           move spaces to tabela-aux.
           perform until tab-reg (sub) = """"   and 
                         tab-reg (sub + 1) = ","
                         move tab-reg (sub) to tab-reg-aux (sub1)
                         add 1 to sub sub1
           end-perform.  
           move tabela-aux to telex.
           add 3 to sub.
           move 1 to sub1.
           move spaces to tabela-aux.
           perform until tab-reg (sub) = """" 
                         move tab-reg (sub) to tab-reg-aux (sub1)
                         add 1 to sub sub1
           end-perform.  
           move tabela-aux to fax.
      *
       copy rotgen.lib.
      *
      ************************
      *                      *
      *       T e l a s      *
      *                      *
      ************************
      *
       telas section.
      *
       accept-resposta-cad.
           move spaces to resposta.
           accept resposta at 1740 with auto foreground-color 04
                                             background-color 04.
           accept escape-key from escape.
           move resposta to txt.
           perform rot-texto.
           move txt to resposta.
      *
       display-erro-usr.
           move " Usuario sem prioridade para esta funcao - Tecle <Enter
      -    ">" to mensagem.
           display tela-erro-cad.
           perform rot-keypress.
      *
       sec-atualizacao section.
      *
       lab-atu-00.
           perform rot-open-at01.
           if erro not = 0
              go to lab-atu-fim
           end-if.
           perform rot-open-ass.
           if erro not = 0
              go to lab-atu-fim
           end-if.
      *
       lab-atu-01.
           move 0 to erro.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-atu-fim
           end-if.
           move ass-codigo to at01-codigo.
           move 0 to erro.
           perform rot-le-at01.
           if erro not = 0
              go to lab-atu-01
           end-if.
           move ass-resto to tabela.
           perform rot-desmembra.
           perform rot-move-at01.
           rewrite reg-at01 invalid key 
                   move 1 to erro
                   move " Erro de regravacao - ARQAT01A.DAT - Tecle <Ent
      -            "er>"
                   to mensagem
                   display tela-erro
                   perform rot-keypress
                   display tela-limpa
           end-rewrite.
           go to lab-atu-01.
      * 
       lab-atu-fim.
           perform rot-close-ass.
           perform rot-close-at01.
           exit.
      *
       sec-deleta section.
      *
       lab-dele-00.
           perform rot-open-at01.
           if erro not = 0
              go to lab-dele-fim
           end-if.
      *
       lab-dele-01.
           move 0 to erro.
           perform rot-le-proximo-at01.
           if erro not = 0
              go to lab-dele-fim
           end-if.
           if at01-chave = high-values
              go to lab-dele-01
           end-if.
           if at01-data not = param-data
              delete arqat01 invalid key 
                     move 1 to erro
                     move " Erro de regravacao - ARQAT01A.DAT - Tecle <E
      -              "nter>" to mensagem
                     display tela-erro
                     perform rot-keypress
                     display tela-limpa
              end-delete
           end-if.
           go to lab-dele-01.
      * 
       lab-dele-fim.
           perform rot-close-at01.
           exit.
      *