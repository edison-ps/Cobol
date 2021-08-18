      ***************************************************************
      *                                                             *
      *  A B A V / S P - I N F O R M A T I C A    :::  QGRC01       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Conversao p/ cobranca bancaria:                            *
      *                                                             *
      *  Data da ultima alteracao:    25/09/95     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. qgrc01.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
      *
           select arqrc01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is rc01-chave
                  alternate record key is rc01-chave-1 with duplicates
                  alternate record key is rc01-chave-2 with duplicates
                  alternate record key is rc01-chave-3 with duplicates
                  file status is rc01-status.
      *
           select arqrc99 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is rc99-chave
                  alternate record key is rc99-chave-1 with duplicates
                  alternate record key is rc99-chave-2 with duplicates
                  file status is rc99-status.
      *
       data division.
       file section.
      *    
       copy fdrc01.lib.
      *
      *  Arquivo de CTAS a Receber - 28/12/93   REG - 128 Bytes
      * 
       fd arqrc99
          label record is standard
          value of file-id is nome-arq-rc99
          data record is reg-rc99.
      *
       01 reg-rc99.
          02 rc99-chave.
             03 rc99-documento         pic x(10).
          02 rc99-chave-1.
             03 rc99-codigo            pic 9(05).
             03 rc99-condicao          pic x(01).
          02 rc99-chave-2.
             03 rc99-vencimento        pic 9(05).
          02 rc99-valor                pic 9(11)v9(02).
          02 rc99-obs                  pic x(30).
          02 rc99-portador             pic 9(03).
          02 rc99-operacao             pic 9(03).
          02 rc99-emissao              pic 9(05).
          02 rc99-usuario              pic x(10).
          02 rc99-data                 pic 9(05).
          02 rc99-filler               pic x(38).
      *
      *   Registro de controle - Chave = High-Values
      *
       01 reg-rc99-1.
          02 rc99-chave-controle.
             03 rc99-controle          pic x(21).
          02 rc99-ult-fat              pic 9(06).
          02 rc99-filler-1             pic x(101).

      *
       working-storage section.
      *
       01 rc01-status                  pic x(02) value "00".
       01 rc01-stat                    pic x(01) value "F".
      *
       01 nome-arq-rc01.
          02 rc01-dir                  pic x(03) value "RC2".
          02 filler                    pic x(01) value "\".
          02 rc01-nome                 pic x(08) value "ARQRC01A".
          02 filler                    pic x(01) value ".".
          02 rc01-ext                  pic x(03) value "DAT".
      *
       01 rc99-status                  pic x(02) value "00".
       01 rc99-stat                    pic x(01) value "F".
      *
       01 nome-arq-rc99.
          02 rc99-dir                  pic x(03) value "TMP".
          02 filler                    pic x(01) value "\".
          02 rc99-nome                 pic x(08) value "ARQRC99A".
          02 filler                    pic x(01) value ".".
          02 rc99-ext                  pic x(03) value "DAT".
      *
       01 total                        pic 9(07) value 0.
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "QGRC01".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       copy workgen.lib.
      *
       screen section.
      *
       copy scrgen.lib.
      *
       procedure division.
      *
       lab-00.
           call "C_Cls".
           perform rot-open-rc99.
           if erro not = 0
              go to lab-fim
           end-if.
           perform rot-open-rc01.
           if erro not = 0
              go to lab-fim
           end-if.
           move low-value to rc99-chave.
           start arqrc99 key is not less rc99-chave.
      *
       lab-01.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-fim
           end-if.
           if rc99-chave = high-values
              move high-values to rc01-chave
              read arqrc01
              move rc99-ult-fat to rc01-ult-fat
              rewrite reg-rc01 invalid key 
                      display
                    " Erro de regravacao - ARQRC99A.DAT - Tecle <Enter>"
                      stop " "
                      go to lab-fim
              end-rewrite
              go to lab-01
           end-if.
           move rc99-codigo to rc01-codigo.
           move rc99-condicao to rc01-condicao.
           move rc99-documento to rc01-documento.
           move spaces to rc01-doc-cob.
           move rc99-vencimento to rc01-vencimento.
           move rc99-valor to rc01-valor.
           move rc99-obs to rc01-obs.
           move rc99-portador to rc01-portador.
           move rc99-operacao to rc01-operacao.
           move rc99-emissao to rc01-emissao
           move rc99-usuario to rc01-usuario.
           move rc99-data to rc01-data.
           write reg-rc01 invalid key 
                 display
                 " Erro de gravacao - ARQRC01A.DAT - Tecle <Enter>"
                 stop " "
                 go to lab-fim
           end-write.
           add 1 to total.
           display total " " rc01-documento.
           go to lab-01.
      *
       lab-fim.
           perform rot-close-rc99.
           perform rot-close-rc01.
           stop run.
      *
      ************************
      *                      *
      *    R o t i n a s     *
      *                      *
      ************************
      *      
       rotinas section.
      *
       rot-open-rc01.
           move 0 to erro.
           if rc01-stat = "F"
              open output arqrc01
              move zeros to reg-rc01-1
              move high-values to rc01-controle
              move 0 to rc01-ult-fat
              write reg-rc01-1
              close arqrc01
              open i-o arqrc01
              if rc01-status not = "00"
                 move 
                 " Erro de abertura no ARQRC01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to rc01-stat
               end-if
           end-if.
      *
       rot-close-rc01.
           if rc01-stat = "A"
              close arqrc01
              move "F" to rc01-stat
           end-if.
      *
       rot-erro-leitura-rc01.
           move " Erro de leitura - ARQRC01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-open-rc99.
           move 0 to erro.
           if rc99-stat = "F"
              open i-o arqrc99
              if rc99-status not = "00"
                 move 
                 " Erro de abertura no ARQRC99A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to rc99-stat
               end-if
           end-if.
      *
       rot-close-rc99.
           if rc99-stat = "A"
              close arqrc99
              move "F" to rc99-stat
           end-if.
      *
       rot-erro-leitura-rc99.
           move " Erro de leitura - ARQRC99A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
      *
       rot-le-proximo.
           move 0 to erro.
           read arqrc99 next at end move 1 to erro.
           if rc99-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       copy rotgen.lib.
      *
