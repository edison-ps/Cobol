      ***************************************************************
      *                                                             *
      *  A B A V / S P -  I N F O R M A T I C A   :::  PGSELE04     *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Menu Principal:                                            *
      *                                                             *
      *  Data da ultima alteracao:    14/12/95     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgsele04.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
            select arqusr assign to disk
                   organization is indexed
                   access mode is random
                   lock mode is manual
                   with lock on record
                   record key is usr-chave
                   file status is usr-status.
      *
            select arqimp assign to disk
                   organization is indexed
                   access mode is dynamic
                   lock mode is manual
                   with lock on record
                   record key is imp-chave
                   file status is imp-status.
      *      
       data division.
       file section.
      *           
       copy fdusr.lib.
      *    
       copy fdimp.lib.
      *
       working-storage section.
      
       01 usr-status                   pic x(02) value "00".
       01 usr-stat                     pic x(01) value "F".
      *
       01 nome-arq-usr.
          02 usr-dir                   pic x(03) value "USR".
          02 filler                    pic x(01) value "\".
          02 usr-nome                  pic x(08) value "ARQUSR00".
          02 filler                    pic x(01) value ".".
          02 usr-ext                   pic x(03) value "DAT".
      *
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 nome-arq-imp.
          02 imp-dir                   pic x(03) value "IMP".
          02 filler                    pic x(01) value "\".
          02 imp-nome                  pic x(07) value "ARQIMPA".
          02 filler                    pic x(01) value ".".
          02 imp-ext                   pic x(03) value "DAT".
      *
       01 cb-prog.
          02 cb-cliente                pic x(40) value
          "ABAV - CN ".
          02 cb-nome                   pic x(18) value
          "ABAV - Informatica".
          02 cb-programa               pic x(08) value "PGSELE04".
          02 cb-versao                 pic x(06) value "v1.00 ".
          02 cb-data                   pic x(08) value spaces.
      *
       01 usuario                      pic x(10) value spaces.
       01 senha                        pic x(10) value spaces.
       01 cont                         pic 9(01) value 0.
       01 opc                          pic s9(02) value 0.
       01 opc-aux                      pic s9(02) value 0.

      *
       01 data-accept                  pic 9(06) value 0.
       01 data-edit redefines data-accept.
          02 edit-ano                  pic 9(02).
          02 edit-mes                  pic 9(02).
          02 edit-dia                  pic 9(02).
      * 
       01 buffer1.
          02 filler                    pic 9(04) occurs 2180.
      * 
       01 buffer2.
          02 filler                    pic 9(04) occurs 300.
      *
       01 spl-dev.
          02 spl                       pic x(03) value "SPL".
          02 filler                    pic x(01) value "\".
          02 dev                       pic x(08) value spaces.
      *
       01 campo-grafico.
          02 filler                    pic x(01) value low-values.
          02 grafico-modo              pic 9(02) comp-5 value 0.
          02 filler                    pic x(01) value low-values.
          02 grafico-arq               pic x(10) value "X.GRF".
          02 filler                    pic x(01) value low-values.
      *
       01 campo-menu.
          02 menu-argum                pic 9(02) comp-5 value 04.
          02 filler                    pic x(01) value low-values.
          02 menu-tam                  pic 9(02) comp-5 value 11.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-f                pic 9(02) comp-5 value 03.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-p                pic 9(02) comp-5 value 08.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-fb               pic 9(02) comp-5 value 03.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-pb               pic 9(02) comp-5 value 14.
          02 filler                    pic x(01) value low-values.
          02 menu-dados.
             03 menu-sentido           pic x(01) value "H".
             03 menu-pos               pic 9(02) comp-5 value 0.
             03 filler                 pic x(01) value x"b3".
             03 filler                 pic 9(02) comp-5 value 08.
             03 filler                 pic 9(02) comp-5 value 06.
             03 filler                 pic x(11) value " Cadastros".
             03 filler                 pic 9(02) comp-5 value 25.
             03 filler                 pic 9(02) comp-5 value 06.
             03 filler                 pic x(11) value "Faturamento".
             03 filler                 pic 9(02) comp-5 value 41.
             03 filler                 pic 9(02) comp-5 value 06.
             03 filler                 pic x(11) value "    Receber".
             03 filler                 pic 9(02) comp-5 value 62.
             03 filler                 pic 9(02) comp-5 value 06.
             03 filler                 pic x(11) value "Utilitarios".
          02 filler                    pic x(01) value low-values.
      *
       01 campo-menu2.
          02 menu-argum2               pic 9(02) comp-5 value 09.
          02 filler                    pic x(01) value low-values.
          02 menu-tam2                 pic 9(02) comp-5 value 17.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-f2               pic 9(02) comp-5 value 01.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-p2               pic 9(02) comp-5 value 02.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-fb2              pic 9(02) comp-5 value 03.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-pb2              pic 9(02) comp-5 value 14.
          02 filler                    pic x(01) value low-values.
          02 menu-dados2.
             03 menu-sentido2          pic x(01) value "V".
             03 menu-pos2              pic 9(02) comp-5 value 0.
             03 filler                 pic x(01) value x"b3".
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic x(17) value "1-Exposicao".
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic 9(02) comp-5 value 12.
             03 filler                 pic x(17) value "2-Estandes".
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic 9(02) comp-5 value 13.
             03 filler                 pic x(17) value "3-Atividades".
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic 9(02) comp-5 value 14.
             03 filler                 pic x(17) value "4-Estados".
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic 9(02) comp-5 value 15.
             03 filler                 pic x(17) value "5-Moedas".
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic 9(02) comp-5 value 16.
             03 filler                 pic x(17) value "6-Cotacoes".
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic 9(02) comp-5 value 17.
             03 filler                 pic x(17) value "7-Relacao".
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic 9(02) comp-5 value 18.
             03 filler                 pic x(17) value "8-Etiquetas".
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic 9(02) comp-5 value 19.
             03 filler                 pic x(17) value "9-Crachas".
          02 filler                    pic x(01) value low-values.
      *
       01 campo-menu3.
          02 menu-argum3               pic 9(02) comp-5 value 05.
          02 filler                    pic x(01) value low-values.
          02 menu-tam3                 pic 9(02) comp-5 value 17.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-f3               pic 9(02) comp-5 value 01.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-p3               pic 9(02) comp-5 value 02.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-fb3              pic 9(02) comp-5 value 03.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-pb3              pic 9(02) comp-5 value 14.
          02 filler                    pic x(01) value low-values.
          02 menu-dados3.
             03 menu-sentido3          pic x(01) value "V".
             03 menu-pos3              pic 9(02) comp-5 value 0.
             03 filler                 pic x(01) value x"b3".
             03 filler                 pic 9(02) comp-5 value 59.
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic x(17) value "1-Usuarios".
             03 filler                 pic 9(02) comp-5 value 59.
             03 filler                 pic 9(02) comp-5 value 12.
             03 filler                 pic x(17) value "2-Impressoras".
             03 filler                 pic 9(02) comp-5 value 59.
             03 filler                 pic 9(02) comp-5 value 13.
             03 filler                 pic x(17) value "3-Backup".
             03 filler                 pic 9(02) comp-5 value 59.
             03 filler                 pic 9(02) comp-5 value 14.
             03 filler                 pic x(17) value "4-Restore".
             03 filler                 pic 9(02) comp-5 value 59.
             03 filler                 pic 9(02) comp-5 value 15.
             03 filler                 pic x(17) value 
             "5-Interface S.O.".
          02 filler                    pic x(01) value low-values.
      *
       01 campo-menu4.
          02 menu-argum4               pic 9(02) comp-5 value 06.
          02 filler                    pic x(01) value low-values.
          02 menu-tam4                 pic 9(02) comp-5 value 23.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-f4               pic 9(02) comp-5 value 01.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-p4               pic 9(02) comp-5 value 02.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-fb4              pic 9(02) comp-5 value 03.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-pb4              pic 9(02) comp-5 value 14.
          02 filler                    pic x(01) value low-values.
          02 menu-dados4.
             03 menu-sentido4          pic x(01) value "V".
             03 menu-pos4              pic 9(02) comp-5 value 0.
             03 filler                 pic x(01) value x"b3".
             03 filler                 pic 9(02) comp-5 value 33.
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic x(23) value "1-Manut. CTAS a 
      -      "Receber".
             03 filler                 pic 9(02) comp-5 value 33.
             03 filler                 pic 9(02) comp-5 value 12.
             03 filler                 pic x(23) value "2-Manut. Portado
      -      "res".
             03 filler                 pic 9(02) comp-5 value 33.
             03 filler                 pic 9(02) comp-5 value 13.
             03 filler                 pic x(23) value "3-Manut. Operaco
      -      "es".
             03 filler                 pic 9(02) comp-5 value 33.
             03 filler                 pic 9(02) comp-5 value 14.
             03 filler                 pic x(23) value "4-Baixa CTAS Rec
      -      "ebidas".
             03 filler                 pic 9(02) comp-5 value 33.
             03 filler                 pic 9(02) comp-5 value 15.
             03 filler                 pic x(23) value "5-Rel. CTAS a Re
      -      "ceber.".
             03 filler                 pic 9(02) comp-5 value 33.
             03 filler                 pic 9(02) comp-5 value 16.
             03 filler                 pic x(23) value "6-Rel. CTAS Rece
      -      "bidas".
          02 filler                    pic x(01) value low-values.
      *
       01 campo-menu5.
          02 menu-argum5               pic 9(02) comp-5 value 01.
          02 filler                    pic x(01) value low-values.
          02 menu-tam5                 pic 9(04) comp-5 value 22.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-f5               pic 9(02) comp-5 value 01.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-p5               pic 9(02) comp-5 value 02.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-fb5              pic 9(02) comp-5 value 03.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-pb5              pic 9(02) comp-5 value 14.
          02 filler                    pic x(01) value low-values.
          02 menu-dados5.
             03 menu-sentido5          pic x(01) value "V".
             03 menu-pos5              pic 9(02) comp-5 value 0.
             03 filler                 pic x(01) value x"b3".
             03 filler                 pic 9(02) comp-5 value 22.
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic x(22) value 
             "1-Rateio de Taxas".
          02 filler                    pic x(01) value low-values.
      *
       01 param-menu.
          02 param-usr                 pic x(10) value spaces.
          02 param-senha               pic x(10) value spaces.
          02 param-prioridade          pic 9(01) value 0.
          02 param-data                pic 9(05) value 0.
          02 param-impress             pic x(12) value spaces.
      *
       01 campo-rotina.
          02 rotina-col                pic 9(02) value 05.
          02 rotina-lin                pic 9(02) value 15.
          02 rotina-borda              pic x(01) value "3".
          02 rotina-fundo              pic x(01) value spaces.
          02 rotina-sombra             pic x(01) value "S".
          02 rotina-tipo               pic 9(02) value 0.
          02 rotina-codigo             pic 9(03) value 0.
      *
       01 result                       pic 9(02) comp-x value 0.
       01 funcao                       pic 9(02) comp-x value 35.
      *
       01 parametro.
          02 parametro-tam             pic 9(02) comp-x value 15.
          02 comando                   pic x(15) value "C:\COMMAND.COM".
      *
       01 parametro-back.
          02 parametro-tam-back        pic 9(02) comp-x value 12.
          02 comando-back              pic x(14) value "C:BACK01.EXE".
      *
       01 parametro-rest.
          02 parametro-tam-rest        pic 9(02) comp-x value 12.
          02 comando-rest              pic x(14) value "C:REST01.EXE".
      *
       copy workgen.lib.
      *
       screen section.
      * 
       01 tela-cabecalho.
          02 line 02 column 03 foreground-color 07 background-color 01
             highlight pic x(40) from cb-cliente.
          02 line 02 column 61 foreground-color 07 background-color 01
             highlight pic x(18) from cb-nome.
          02 line 24 column 01 foreground-color 07 background-color 01
             highlight pic x(80) from spaces.
      *
        01 tela-rodape.
          02 line 24 column 02 foreground-color 07 background-color 01
             highlight pic x(08) from cb-programa.
          02 line 24 column 12 foreground-color 07 background-color 01
             highlight pic x(08) from cb-versao.
          02 line 24 column 30 foreground-color 07 background-color 01
             value "Usr.:".
          02 line 24 column 36 foreground-color 07 background-color 01
             highlight pic x(08) from usr-usuario.
          02 line 24 column 50 foreground-color 07 background-color 01
             value "Imp.:".
          02 line 24 column 56 foreground-color 07 background-color 01
             highlight pic x(08) from param-impress.
          02 line 24 column 70 foreground-color 07 background-color 01
             highlight pic x(08) from cb-data.         

      *
       01 tela-01.
          02 line 13 column 31 foreground-color 07 background-color 03
             highlight value "Usuario :".
          02 line 13 column 41 foreground-color 05 background-color 01
             pic x(10) from spaces.
          02 line 15 column 31 foreground-color 07 background-color 03
             highlight value "Senha   :".
          02 line 15 column 41 foreground-color 05 background-color 01
             pic x(10) from spaces.
      *
       copy scrgen.lib.
      *
       procedure division using param-menu campo-rotina.
      *
       lab-00.
      *     move 4 to grafico-modo.
      *     call "C_SetC_modo" using by value grafico-modo.
      *     call "C_MoveC_image" using by reference grafico-arq.
      *     move 0 to box-lin.
      *     move 0 to box-col.
      *     call "C_Gotoxy" using by value box-col
      *                           by value box-lin.
      *     perform rot-keypress.
      *     move 3 to grafico-modo.
      *     call "C_SetC_modo" using by value grafico-modo.
           move 07 to box-cor-f.
           move 09 to box-cor-p.
           move x"b2" to box-fundo.
           perform rot-box.
           move 02 to box-lin-f.
           move 01 to box-cor-f.
           move 07 to box-cor-p.
           move 1 to box-borda.
           move spaces to box-fundo.
           perform rot-box.
           display tela-cabecalho.
           move 28 to box-col.
           move 11 to box-lin.
           move 54 to box-col-f.
           move 17 to box-lin-f.
           perform rot-save-buffer.
           subtract 2 from box-col-f box-lin-f.
           move 03 to box-cor-f.
           move 15 to box-cor-p.
           move "S" to box-sombra.
           perform rot-box.
      *
       lab-01.
           display tela-01.
           move spaces to usuario.
           perform acc-usuario.
           if escape-key = 1
              go to lab-fim.
           if usuario = spaces
              go to lab-01.
           move usuario to txt.
           perform rot-texto.
           move txt to usuario.
           perform dsp-usuario.
           if usr-stat = "F"
              open input arqusr
              if usr-status not = "00"
                 close arqusr
                 move 
                 " Erro de abertura no ARQUSR00.DAT - Tecle <Enter>" to 
                  mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-fim
              end-if
              move "A" to usr-stat
           end-if.
           move usuario to usr-usuario.
      *
       lab-02.
           move 0 to erro.
           read arqusr invalid key move 1 to erro.
           if usr-status = "9D"
              call "C_Wait" using by value campo-wait
              go to lab-02
           end-if.
           if erro not = 0
              move " Usuario nao cadastrado - Tecle <Enter>" to mensagem
              display tela-erro
              perform rot-keypress
              display tela-limpa
              go to lab-01
           end-if.
           move 0 to cont.
           move usuario to param-usr.
           move senha to param-senha.
           move usr-prioridade to param-prioridade.
           accept data-accept from date.
           move edit-ano to ano-euro.
           move edit-mes to mes-euro.
           move edit-dia to dia-euro.
           move "4" to opcao-data.
           perform rot-data.
           move dias-corr to param-data.
           move data-disp to cb-data.
           if imp-stat = "F"
              open i-o arqimp
              if imp-status not = "00"
                 move 
                 " Erro de abertura no ARQIMPA.DAT - Tecle <Enter>" to 
                  mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 go to lab-fim
              end-if
              move "A" to imp-stat
           end-if.
           move usr-impress to imp-impress
           perform rot-le-imp.
           if erro not = 0
              move usuario to dev
              move spl-dev to param-impress
           else
              move imp-device to param-impress
           end-if.
           if imp-stat = "A"
              close arqimp 
              move "F" to imp-stat
           end-if.
      *
       lab-03.
           add 1 to cont.
           perform lmp-senha.
           perform acc-senha.
           if escape-key = 1
              go to lab-01
           end-if.
           if senha = spaces
              subtract 1 from cont
              go to lab-03
           end-if.
           move 1 to cript-opcao.
           move 10 to cript-tam.
           move spaces to campo-cript.
           move senha to cript-txt.
           perform rot-cript.
           move cript-txt to senha.
           if senha not = usr-senha
              move " Senha incorreta - Tecle <Enter>" to mensagem
              display tela-erro
              perform rot-keypress
              display tela-limpa
              if cont < 3
                 go to lab-03
              else
                 go to lab-fim
              end-if
           end-if.
           close arqusr.
           move "F" to usr-stat.
           add 2 to box-col-f box-lin-f.
           perform rot-rest-buffer.
           display tela-rodape.
           move 05 to box-col.
           move 05 to box-lin.
           move 75 to box-col-f.
           move 07 to box-lin-f.
           move "2" to box-borda.
           move 03 to box-cor-f.
           move 15 to box-cor-p.
           perform rot-box.
           perform until opc  = -1 
                   call "C_Menu" using by value menu-argum
                                       by value menu-tam
                                       by value menu-cor-f
                                       by value menu-cor-p
                                       by value menu-cor-fb
                                       by value menu-cor-pb
                                       by reference menu-dados
                   move return-code to opc
                   move 0 to box-col box-lin
                   move 80 to box-col-f
                   move 25 to box-lin-f
                   perform rot-save-buffer
                   evaluate true
                            when opc = 1
                                 perform rot-cadastro
                            when opc = 2
                                 perform rot-fatura
                            when opc = 3
                                 perform rot-receber
                            when opc = 4
                                 perform rot-util
                   end-evaluate
                   move 0 to box-col box-lin
                   move 80 to box-col-f
                   move 25 to box-lin-f
                   if opc not = -1 
                      perform rot-rest-buffer
                      display tela-rodape
                   end-if
           end-perform.

      *
       lab-fim.
           if usr-stat = "A"
              close arqusr
           end-if.
           call "C_Cls".
           move 01 to box-lin box-col
           call "C_Gotoxy" using by value box-col
                                 by value box-lin.
           stop run.
      *
      ************************
      *                      *
      *    R o t i n a s     *
      *                      *
      ************************
      *      
       rotina section.
      *
       rot-save-buffer1.
           call "C_Savescr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer1.
      * 
       rot-rest-buffer1.
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer1.
      *
       rot-save-buffer2.
           call "C_Savescr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer2.
      * 
       rot-rest-buffer2.
           call "C_Restscr" using by value box-col
                                  by value box-lin
                                  by value box-col-f
                                  by value box-lin-f
                                  by reference buffer2.
      *
       rot-le-imp.
           move 0 to erro.
           read arqimp invalid key move 1 to erro.
           if imp-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-imp.
      *
       rot-cadastro.
           move 10 to box-col.
           move 10 to box-lin.
           move 28 to box-col-f.
           move 20 to box-lin-f.
           move "3" to box-borda.
           move 01 to box-cor-f.
           move 15 to box-cor-p.
           perform rot-box.
           move 0 to opc-aux.
           perform until opc-aux = -1
                   call "C_Menu" using by value menu-argum2
                                       by value menu-tam2
                                       by value menu-cor-f2
                                       by value menu-cor-p2
                                       by value menu-cor-fb2
                                       by value menu-cor-pb2
                                       by reference menu-dados2
                   move return-code to opc-aux
                   move 0 to box-col box-lin
                   move 79 to box-col-f
                   move 24 to box-lin-f
                   perform rot-save-buffer1
                   evaluate true
                            when opc-aux = 2
                                 call "pgex01" using param-menu 
                                 cancel "pgex01"
                            when opc-aux = 3
                                 move 15 to rotina-col
                                 move 11 to rotina-lin
                                 move "3" to rotina-borda
                                 move spaces to rotina-fundo
                                 move "S" to rotina-sombra
                                 move 1 to rotina-tipo
                                 call "pgtab01" using param-menu 
                                                      campo-rotina
                                 cancel "pgtab01"
                            when opc-aux = 4
                                 move 15 to rotina-col
                                 move 11 to rotina-lin
                                 move "3" to rotina-borda
                                 move spaces to rotina-fundo
                                 move "S" to rotina-sombra
                                 move 3 to rotina-tipo
                                 call "pgtab02" using param-menu 
                                                      campo-rotina
                                 cancel "pgtab02"
                            when opc-aux = 5
                                 move 15 to rotina-col
                                 move 11 to rotina-lin
                                 move "3" to rotina-borda
                                 move spaces to rotina-fundo
                                 move "S" to rotina-sombra
                                 move 10 to rotina-tipo
                                 call "pgtab05" using param-menu 
                                                      campo-rotina
                                 cancel "pgtab05"
                            when opc-aux = 6
                                 move 15 to rotina-col
                                 move 11 to rotina-lin
                                 move "3" to rotina-borda
                                 move spaces to rotina-fundo
                                 move "S" to rotina-sombra
                                 move 11 to rotina-tipo
                                 call "pgtab06" using param-menu 
                                                      campo-rotina
                                 cancel "pgtab06"
                   end-evaluate
                   display tela-rodape
                   if opc-aux not = -1
                      move 0 to box-col box-lin
                      move 79 to box-col-f
                      move 24 to box-lin-f
                      perform rot-rest-buffer1
                      display tela-rodape
                   end-if
           end-perform.
      *
       rot-util.
           move 58 to box-col.
           move 10 to box-lin.
           move 76 to box-col-f.
           move 16 to box-lin-f.
           move "3" to box-borda.
           move 01 to box-cor-f.
           move 15 to box-cor-p.
           perform rot-box.
           move 0 to opc-aux.
           perform until opc-aux = -1
                   call "C_Menu" using by value menu-argum3
                                       by value menu-tam3
                                       by value menu-cor-f3
                                       by value menu-cor-p3
                                       by value menu-cor-fb3
                                       by value menu-cor-pb3
                                       by reference menu-dados3
                   move return-code to opc-aux
                   add 1 to box-col-f box-lin-f
                   perform rot-save-buffer1
                   evaluate true
                            when opc-aux = 1
                                 move 4 to rotina-col
                                 move 11 to rotina-lin
                                 move "3" to rotina-borda
                                 move spaces to rotina-fundo
                                 move "S" to rotina-sombra
                                 move 1 to rotina-tipo
                                 call "pgusr01" using param-menu 
                                                      campo-rotina
                                 cancel "pgusr01"
                            when opc-aux = 2
                                 move 4 to rotina-col
                                 move 11 to rotina-lin
                                 move "3" to rotina-borda
                                 move spaces to rotina-fundo
                                 move "S" to rotina-sombra
                                 move 0 to rotina-tipo
                                 call "pgimp01" using param-menu 
                                                      campo-rotina
                                 cancel "pgimp01"
                                
                            when opc-aux = 3
                                 perform rot-back
                            when opc-aux = 4
                                 perform rot-rest
                            when opc-aux = 5
                                 perform rot-interface
                   end-evaluate
                   display tela-rodape
                   if opc-aux not = -1
                      perform rot-rest-buffer1
                      display tela-rodape
                   end-if
           end-perform.
      *
       rot-fatura.
           move 21 to box-col.
           move 10 to box-lin.
           move 44 to box-col-f.
           move 12 to box-lin-f.
           move "3" to box-borda.
           move 01 to box-cor-f.
           move 15 to box-cor-p.
           perform rot-box.
           move 0 to opc-aux.
           perform until opc-aux = -1
                   call "C_Menu" using by value menu-argum5
                                       by value menu-tam5
                                       by value menu-cor-f5
                                       by value menu-cor-p5
                                       by value menu-cor-fb5
                                       by value menu-cor-pb5
                                       by reference menu-dados5
                   move return-code to opc-aux
                   add 1 to box-col-f box-lin-f
                   perform rot-save-buffer1
                   evaluate true
                            when opc-aux = 1
                                 perform rot-keypress
                   end-evaluate
                   display tela-rodape
                   if opc-aux not = -1
                      perform rot-rest-buffer1
                      display tela-rodape
                   end-if
           end-perform.
      *
       rot-receber.
           move 32 to box-col.
           move 10 to box-lin.
           move 56 to box-col-f.
           move 17 to box-lin-f.
           move "3" to box-borda.
           move 01 to box-cor-f.
           move 15 to box-cor-p.
           perform rot-box.
           move 0 to opc-aux.
           perform until opc-aux = -1
                   call "C_Menu" using by value menu-argum4
                                       by value menu-tam4
                                       by value menu-cor-f4
                                       by value menu-cor-p4
                                       by value menu-cor-fb4
                                       by value menu-cor-pb4
                                       by reference menu-dados4
                   move return-code to opc-aux
                   add 1 to box-col-f box-lin-f
                   perform rot-save-buffer1
                   evaluate true
                            when opc-aux = 1
                                 call "pgre01" using param-menu 
                                 cancel "pgre01"
                            when opc-aux = 2
                                 move 05 to rotina-col
                                 move 11 to rotina-lin
                                 move "3" to rotina-borda
                                 move spaces to rotina-fundo
                                 move "S" to rotina-sombra
                                 move 4 to rotina-tipo
                                 call "pgtab01" using param-menu 
                                                      campo-rotina
                                 cancel "pgtab01"
                            when opc-aux = 3
                                 move 05 to rotina-col
                                 move 11 to rotina-lin
                                 move "3" to rotina-borda
                                 move spaces to rotina-fundo
                                 move "S" to rotina-sombra
                                 move 5 to rotina-tipo
                                 call "pgtab01" using param-menu 
                                                      campo-rotina
                                 cancel "pgtab01"
                            when opc-aux = 4
                                 call "pgre02" using param-menu 
                                 cancel "pgre02"
                            when opc-aux = 5
                                 call "pgre03" using param-menu 
                                 cancel "pgre03"
                            when opc-aux = 6
                                 call "pgre04" using param-menu 
                                 cancel "pgre04"
                   end-evaluate
                   display tela-rodape
                   if opc-aux not = -1
                      perform rot-rest-buffer1
                      display tela-rodape
                   end-if
           end-perform.
      *
       rot-back.
           move 0 to box-col box-lin.
           move 80 to box-col-f.
           move 25 to box-lin-f.
           perform rot-save-buffer1.
           move 00 to result.
           move 35 to funcao.
           move 12 to parametro-tam-back.
           call "C_Cls".
           move 0 to box-col.
           move 0 to box-lin.
           call "C_Gotoxy" using by value box-col
                                 by value box-lin.
           move "C:BACK04.EXE" to comando-back.
           call x"91" using result funcao parametro-back.
           perform rot-keypress.
           perform rot-rest-buffer1.
      *
       rot-rest.
           move 0 to box-col box-lin.
           move 80 to box-col-f.
           move 25 to box-lin-f.
           perform rot-save-buffer1.
           move 00 to result.
           move 35 to funcao.
           move 12 to parametro-tam-rest.
           call "C_Cls".
           move 0 to box-col.
           move 0 to box-lin.
           call "C_Gotoxy" using by value box-col
                                 by value box-lin.
           move "C:REST04.EXE" to comando-rest.
           call x"91" using result funcao parametro-rest.
           perform rot-keypress.
           perform rot-rest-buffer1.
      *
       rot-interface.
           move 0 to box-col box-lin.
           move 80 to box-col-f.
           move 25 to box-lin-f.
           perform rot-save-buffer1.
           move 00 to result.
           move 35 to funcao.
           move 15 to parametro-tam.
           call "C_Cls".
           move 0 to box-col.
           move 0 to box-lin.
           call "C_Gotoxy" using by value box-col
                                 by value box-lin.
           move spaces to resposta.
           display resposta upon command-line.
           move "C:\COMMAND.COM" to comando.
           call x"91" using result funcao parametro.
           perform rot-rest-buffer1.
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
       acc-usuario.
           accept usuario at 1341 with auto update foreground-color 15
                                       background-color 01.
           accept escape-key from escape.
           exit.
      *
       dsp-usuario.
           display usuario at 1341 with foreground-color 15
                                        background-color 01.
      *
       acc-senha.
           accept senha at 1541 with auto secure foreground-color 15
                                     background-color 01.
           accept escape-key from escape.
           exit.
      *
       lmp-senha.
           move spaces to senha.
           display senha at 1541 with foreground-color 15
                                      background-color 01.
      *
