      ***************************************************************
      *                                                             *
      *  A B A V / S P -  I N F O R M A T I C A   :::  PGSELE01     *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Menu Principal:                                            *
      *                                                             *
      *  Data da ultima alteracao:    16/11/93     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgsele01.
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
          "ABAV - SP ".
          02 cb-nome                   pic x(18) value
          "ABAV - Informatica".
          02 cb-programa               pic x(08) value "PGSELE01".
          02 cb-versao                 pic x(06) value "v1.00 ".
          02 cb-data                   pic x(08) value spaces.
      *
       01 usuario                      pic x(10) value spaces.
       01 senha                        pic x(10) value spaces.
       01 cont                         pic 9(01) value 0.
       01 opc                          pic s9(02) value 0.
       01 opc-aux                      pic s9(02) value 0.
       01 flag-int                     pic 9(01) value 0.

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
          02 filler                    pic 9(04) occurs 500.
      *
       01 spl-dev.
          02 spl                       pic x(03) value "SPL".
          02 filler                    pic x(01) value "\".
          02 dev                       pic x(08) value spaces.
      *
       01 campo-menu.
          02 menu-argum                pic 9(02) comp-5 value 05.
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
             03 filler                 pic 9(02) comp-5 value 22.
             03 filler                 pic 9(02) comp-5 value 06.
             03 filler                 pic x(11) value "Faturamento".
             03 filler                 pic 9(02) comp-5 value 33.
             03 filler                 pic 9(02) comp-5 value 06.
             03 filler                 pic x(11) value "    Receber".
             03 filler                 pic 9(02) comp-5 value 48.
             03 filler                 pic 9(02) comp-5 value 06.
             03 filler                 pic x(11) value "  S.P.T.".
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
             03 filler                 pic x(17) value "1-Associados".
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic 9(02) comp-5 value 12.
             03 filler                 pic x(17) value "2-Categorias".
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic 9(02) comp-5 value 13.
             03 filler                 pic x(17) value "3-Estados".
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic 9(02) comp-5 value 14.
             03 filler                 pic x(17) value "4-Balcao".
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic 9(02) comp-5 value 15.
             03 filler                 pic x(17) value "5-Rel. Associado
      -      "s".
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic 9(02) comp-5 value 16.
             03 filler                 pic x(17) value "6-Rel. Titulares
      -      " ".
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic 9(02) comp-5 value 17.
             03 filler                 pic x(17) value "7-Etiquetas".
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic 9(02) comp-5 value 18.
             03 filler                 pic x(17) value "8-Quadro Estat.
      -      " ".
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic 9(02) comp-5 value 19.
             03 filler                 pic x(17) value "9-Mala Direta".
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
          02 menu-argum4               pic 9(02) comp-5 value 07.
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
             03 filler                 pic 9(02) comp-5 value 33.
             03 filler                 pic 9(02) comp-5 value 17.
             03 filler                 pic x(23) value "7-Limpeza Recebi
      -      "mentos".
          02 filler                    pic x(01) value low-values.
      *
       01 campo-menu5.
          02 menu-argum5               pic 9(02) comp-5 value 06.
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
             "1-Calculo Mensalidade".
             03 filler                 pic 9(02) comp-5 value 22.
             03 filler                 pic 9(02) comp-5 value 12.
             03 filler                 pic x(22) value 
             "2-Calculos Balcao".
             03 filler                 pic 9(02) comp-5 value 22.
             03 filler                 pic 9(02) comp-5 value 13.
             03 filler                 pic x(22) value 
             "3-Fichas de Cobranca".
             03 filler                 pic 9(02) comp-5 value 22.
             03 filler                 pic 9(02) comp-5 value 14.
             03 filler                 pic x(22) value 
             "4-Recibos do Balcao".
             03 filler                 pic 9(02) comp-5 value 22.
             03 filler                 pic 9(02) comp-5 value 15.
             03 filler                 pic x(22) value 
             "5-Doctos. de Cobranca".
             03 filler                 pic 9(02) comp-5 value 22.
             03 filler                 pic 9(02) comp-5 value 16.
             03 filler                 pic x(22) value 
             "6-Calculos Diversos".
          02 filler                    pic x(01) value low-values.
      *
       01 campo-menu6.
          02 menu-argum6               pic 9(02) comp-5 value 06.
          02 filler                    pic x(01) value low-values.
          02 menu-tam6                 pic 9(02) comp-5 value 23.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-f6               pic 9(02) comp-5 value 01.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-p6               pic 9(02) comp-5 value 02.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-fb6              pic 9(02) comp-5 value 03.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-pb6              pic 9(02) comp-5 value 14.
          02 filler                    pic x(01) value low-values.
          02 menu-dados6.
             03 menu-sentido6          pic x(01) value "V".
             03 menu-pos6              pic 9(02) comp-5 value 0.
             03 filler                 pic x(01) value x"b3".
             03 filler                 pic 9(02) comp-5 value 45.
             03 filler                 pic 9(02) comp-5 value 11.
             03 filler                 pic x(23) value "1-Participantes
      -      " ".
             03 filler                 pic 9(02) comp-5 value 45.
             03 filler                 pic 9(02) comp-5 value 12.
             03 filler                 pic x(23) value "2-Quadro de Insc
      -      "ricoes ".
             03 filler                 pic 9(02) comp-5 value 45.
             03 filler                 pic 9(02) comp-5 value 13.
             03 filler                 pic x(23) value "3-Rel. de Partic
      -      "imantes".
             03 filler                 pic 9(02) comp-5 value 45.
             03 filler                 pic 9(02) comp-5 value 14.
             03 filler                 pic x(23) value "4-Etiquetas
      -      "S.P.T.".
             03 filler                 pic 9(02) comp-5 value 45.
             03 filler                 pic 9(02) comp-5 value 15.
             03 filler                 pic x(23) value "5-Backup
      -      "S.P.T.".
             03 filler                 pic 9(02) comp-5 value 45.
             03 filler                 pic 9(02) comp-5 value 16.
             03 filler                 pic x(23) value "6-Restore
      -      "S.P.T.".
          02 filler                    pic x(01) value low-values.
      *
       01 campo-menu7.
          02 menu-argum7               pic 9(02) comp-5 value 03.
          02 filler                    pic x(01) value low-values.
          02 menu-tam7                 pic 9(02) comp-5 value 16.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-f7               pic 9(02) comp-5 value 01.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-p7               pic 9(02) comp-5 value 02.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-fb7              pic 9(02) comp-5 value 03.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-pb7              pic 9(02) comp-5 value 14.
          02 filler                    pic x(01) value low-values.
          02 menu-dados7.
             03 menu-sentido7          pic x(01) value "V".
             03 menu-pos7              pic 9(02) comp-5 value 0.
             03 filler                 pic x(01) value x"b3".
             03 filler                 pic 9(02) comp-5 value 19.
             03 filler                 pic 9(02) comp-5 value 14.
             03 filler                 pic x(16) value "1-Cadastro".
             03 filler                 pic 9(02) comp-5 value 19.
             03 filler                 pic 9(02) comp-5 value 15.
             03 filler                 pic x(16) value "2-Relacao".
             03 filler                 pic 9(02) comp-5 value 19.
             03 filler                 pic 9(02) comp-5 value 16.
             03 filler                 pic x(16) value "3-Etiquetas".
          02 filler                    pic x(01) value low-values.
      *
       01 campo-menu8.
          02 menu-argum8               pic 9(02) comp-5 value 08.
          02 filler                    pic x(01) value low-values.
          02 menu-tam8                 pic 9(02) comp-5 value 17.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-f8               pic 9(02) comp-5 value 01.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-p8               pic 9(02) comp-5 value 02.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-fb8              pic 9(02) comp-5 value 03.
          02 filler                    pic x(01) value low-values.
          02 menu-cor-pb8              pic 9(02) comp-5 value 14.
          02 filler                    pic x(01) value low-values.
          02 menu-dados8.
             03 menu-sentido8          pic x(01) value "V".
             03 menu-pos8              pic 9(02) comp-5 value 0.
             03 filler                 pic x(01) value x"b3".
             03 filler                 pic 9(02) comp-5 value 19.
             03 filler                 pic 9(02) comp-5 value 13.
             03 filler                 pic x(17) value "1-Consorciadas".
             03 filler                 pic 9(02) comp-5 value 19.
             03 filler                 pic 9(02) comp-5 value 14.
             03 filler                 pic x(17) value "2-Cidades".
             03 filler                 pic 9(02) comp-5 value 19.
             03 filler                 pic 9(02) comp-5 value 15.
             03 filler                 pic x(17) value "3-Cias. Aereas".
             03 filler                 pic 9(02) comp-5 value 19.
             03 filler                 pic 9(02) comp-5 value 16.
             03 filler                 pic x(17) value "4-T.K.T.'S".
             03 filler                 pic 9(02) comp-5 value 19.
             03 filler                 pic 9(02) comp-5 value 17.
             03 filler                 pic x(17) value "5-Rel. Balcao".
             03 filler                 pic 9(02) comp-5 value 19.
             03 filler                 pic 9(02) comp-5 value 18.
             03 filler                 pic x(17) value "6-Rel. T.K.T.'S
      -      " ".
             03 filler                 pic 9(02) comp-5 value 19.
             03 filler                 pic 9(02) comp-5 value 19.
             03 filler                 pic x(17) value "7-Backup ".
             03 filler                 pic 9(02) comp-5 value 19.
             03 filler                 pic 9(02) comp-5 value 20.
             03 filler                 pic x(17) value "8-Restore ".
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
       01 campo-rotina-ab02.
          02 rotina-ab02-codigo        pic 9(05) value 0.
          02 rotina-ab02-nivel         pic 9(01) value 0.
      *
       01 result                       pic 9(02) comp-x value 0.
       01 funcao                       pic 9(02) comp-x value 35.
      *
       01 parametro.
          02 parametro-tam             pic 9(02) comp-x value 15.
          02 comando                   pic x(15) value spaces.
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
                                 perform rot-spt
                            when opc = 5
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
                            when opc-aux = 1
                                 call "pgab01" using param-menu 
                                 cancel "pgab01"
                            when opc-aux = 2
                                 move 15 to rotina-col
                                 move 11 to rotina-lin
                                 move "3" to rotina-borda
                                 move spaces to rotina-fundo
                                 move "S" to rotina-sombra
                                 move 1 to rotina-tipo
                                 call "pgtab01" using param-menu 
                                                      campo-rotina
                                 cancel "pgtab01"
                            when opc-aux = 3
                                 move 15 to rotina-col
                                 move 11 to rotina-lin
                                 move "3" to rotina-borda
                                 move spaces to rotina-fundo
                                 move "S" to rotina-sombra
                                 move 3 to rotina-tipo
                                 call "pgtab02" using param-menu 
                                                      campo-rotina
                                 cancel "pgtab02"
                            when opc-aux = 4
                                 perform rot-balcao
                                 move 0 to opc-aux
                            when opc-aux = 5
                                 call "pgab06" using param-menu 
                                 cancel "pgab06"
                            when opc-aux = 6
                                 call "pgab09" using param-menu 
                                 cancel "pgab09"
                            when opc-aux = 7
                                 call "pgab07" using param-menu 
                                 cancel "pgab07"
                            when opc-aux = 8
                                 call "pgab08" using param-menu 
                                 cancel "pgab08"
                            when opc-aux = 9
                                 perform rot-mala
                                 move 0 to opc-aux
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
                                 move 1 to flag-int
                                 perform rot-interface
                            when opc-aux = 4
                                 move 2 to flag-int
                                 perform rot-interface
                            when opc-aux = 5
                                 move 0 to flag-int
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
           move 17 to box-lin-f.
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
                                 call "pgft01" using param-menu 
                                 cancel "pgft01"
                            when opc-aux = 2
                                 call "pgft06" using param-menu 
                                 cancel "pgft06"
                            when opc-aux = 3
                                 call "pgft02" using param-menu 
                                 cancel "pgft02"
                            when opc-aux = 4
                                 call "pgft04" using param-menu 
                                 cancel "pgft04"
                            when opc-aux = 5
                                 call "pgft05" using param-menu 
                                 cancel "pgft05"
                            when opc-aux = 6
                                 call "pgft03" using param-menu 
                                 cancel "pgft03"
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
           move 18 to box-lin-f.
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
                                 call "pgrc01" using param-menu 
                                 cancel "pgrc01"
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
                                 call "pgrc02" using param-menu 
                                 cancel "pgrc02"
                            when opc-aux = 5
                                 call "pgrc03" using param-menu 
                                 cancel "pgrc03"
                            when opc-aux = 6
                                 call "pgrc04" using param-menu 
                                 cancel "pgrc04"
      *                      when opc-aux = 7

                   end-evaluate
                   display tela-rodape
                   if opc-aux not = -1
                      perform rot-rest-buffer1
                      display tela-rodape
                   end-if
           end-perform.
      *
       rot-spt.
           move 44 to box-col.
           move 10 to box-lin.
           move 68 to box-col-f.
           move 17 to box-lin-f.
           move "3" to box-borda.
           move 01 to box-cor-f.
           move 15 to box-cor-p.
           perform rot-box.
           move 0 to opc-aux.
           perform until opc-aux = -1
                   call "C_Menu" using by value menu-argum6
                                       by value menu-tam6
                                       by value menu-cor-f6
                                       by value menu-cor-p6
                                       by value menu-cor-fb6
                                       by value menu-cor-pb6
                                       by reference menu-dados6
                   move return-code to opc-aux
                   add 1 to box-col-f box-lin-f
                   perform rot-save-buffer1
                   evaluate true
                            when opc-aux = 1
                                 call "pgsl01" using param-menu 
                                 cancel "pgsl01"
                            when opc-aux = 2
                                 call "pgsl02" using param-menu 
                                 cancel "pgsl02"
                            when opc-aux = 3
                                 call "pgsl03" using param-menu 
                                 cancel "pgsl03"
                            when opc-aux = 4
                                 call "pgsl04" using param-menu 
                                 cancel "pgsl04"
                            when opc-aux = 5
                                 move 3 to flag-int
                                 perform rot-interface
                            when opc-aux = 6
                                 move 4 to flag-int
                                 perform rot-interface
                   end-evaluate
                   display tela-rodape
                   if opc-aux not = -1
                      perform rot-rest-buffer1
                      display tela-rodape
                   end-if
           end-perform.
      *
       rot-mala.
           move 18 to box-col.
           move 13 to box-lin.
           move 35 to box-col-f.
           move 17 to box-lin-f.
           move "3" to box-borda.
           move 01 to box-cor-f.
           move 15 to box-cor-p.
           perform rot-box.
           move 0 to opc-aux.
           perform until opc-aux = -1
                   call "C_Menu" using by value menu-argum7
                                       by value menu-tam7
                                       by value menu-cor-f7
                                       by value menu-cor-p7
                                       by value menu-cor-fb7
                                       by value menu-cor-pb7
                                       by reference menu-dados7
                   move return-code to opc-aux
                   add 1 to box-col-f box-lin-f
                   perform rot-save-buffer2
                   evaluate true
                            when opc-aux = 1
                                 call "pgcd01" using param-menu
                                 cancel "pgcd01"
                            when opc-aux = 2
                                 call "pgcd03" using param-menu
                                 cancel "pgcd03"
                            when opc-aux = 3
                                 call "pgcd02" using param-menu
                                 cancel "pgcd02"
                   end-evaluate
                   display tela-rodape
                   if opc-aux not = -1
                      perform rot-rest-buffer2
                      display tela-rodape
                   end-if
           end-perform.
      *
       rot-balcao.
           move 18 to box-col.
           move 12 to box-lin.
           move 36 to box-col-f.
           move 21 to box-lin-f.
           move "3" to box-borda.
           move 01 to box-cor-f.
           move 15 to box-cor-p.
           perform rot-box.
           move 0 to opc-aux.
           perform until opc-aux = -1
                   call "C_Menu" using by value menu-argum8
                                       by value menu-tam8
                                       by value menu-cor-f8
                                       by value menu-cor-p8
                                       by value menu-cor-fb8
                                       by value menu-cor-pb8
                                       by reference menu-dados8
                   move return-code to opc-aux
                   add 1 to box-col-f box-lin-f
                   perform rot-save-buffer2
                   evaluate true
                            when opc-aux = 1
                                 call "pgab02" using param-menu 
                                                     campo-rotina-ab02
                                 cancel "pgab02"
                            when opc-aux = 2
                                 move 15 to rotina-col
                                 move 11 to rotina-lin
                                 move "3" to rotina-borda
                                 move spaces to rotina-fundo
                                 move "S" to rotina-sombra
                                 move 8 to rotina-tipo
                                 call "pgtab03" using param-menu 
                                                      campo-rotina
                                 cancel "pgtab03"
                            when opc-aux = 3
                                 move 15 to rotina-col
                                 move 11 to rotina-lin
                                 move "3" to rotina-borda
                                 move spaces to rotina-fundo
                                 move "S" to rotina-sombra
                                 move 9 to rotina-tipo
                                 call "pgtab04" using param-menu 
                                                      campo-rotina
                                 cancel "pgtab04"
                            when opc-aux = 4
                                 call "pgab0a" using param-menu
                                 cancel "pgab0a"
                            when opc-aux = 5
                                 call "pgab03" using param-menu
                                 cancel "pgab03"
                            when opc-aux = 6
                                 call "pgab0b" using param-menu
                                 cancel "pgab0b"
                            when opc-aux = 7
                                 move 5 to flag-int
                                 perform rot-interface
                            when opc-aux = 8
                                 move 6 to flag-int
                                 perform rot-interface
                   end-evaluate
                   display tela-rodape
                   if opc-aux not = -1
                      perform rot-rest-buffer2
                      display tela-rodape
                   end-if
           end-perform.
      *
       rot-interface.
           move 0 to box-col box-lin.
           move 80 to box-col-f.
           move 25 to box-lin-f.
           perform rot-save-buffer1.
           move 00 to result.
           move 35 to funcao.
           move 13 to parametro-tam.
           evaluate true
                    when flag-int = 0
                         move 15 to parametro-tam
                         move "C:\COMMAND.COM" to comando
                    when flag-int = 1
                         move 12 to parametro-tam
                         move "C:BACK01.EXE" to comando
                    when flag-int = 2
                         move 12 to parametro-tam
                         move "C:REST01.EXE" to comando
                    when flag-int = 3
                         move "C:BACKSPT.EXE" to comando
                    when flag-int = 4   
                         move "C:RESTSPT.EXE" to comando
                    when flag-int = 5
                         move "C:BACKBLC.EXE" to comando
                    when flag-int = 6
                         move "C:RESTBLC.EXE" to comando
           end-evaluate.
           call "C_Cls".
           move 0 to box-col.
           move 0 to box-lin.
           call "C_Gotoxy" using by value box-col
                                 by value box-lin.
           call x"91" using result funcao parametro.
           if flag-int > 0
              perform rot-keypress
           end-if.
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