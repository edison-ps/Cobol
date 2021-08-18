      ***************************************************************
      *                                                             *
      *  A B A V / S P -  I N F O R M A T I C A   :::  PGSL02       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Quadro de Inscricoes :                                     *
      *                                                             *
      *  Data da ultima alteracao:    05/04/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. pgsl02.
       author. Edisom Pires de Souza.
      *
       environment division.
           configuration section.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
           select arqsl01 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is sl01-chave
                  alternate record key is sl01-chave-1 with duplicates
                  alternate record key is sl01-chave-2 with duplicates
                  alternate record key is sl01-chave-3 with duplicates
                  alternate record key is sl01-chave-4 with duplicates
                  file status is sl01-status.
      *
           select arqimp assign to printer
                  organization is line sequential
                  lock mode is manual
                  file status is imp-status.
      *
       data division.
       file section.
      *    
       copy fdsl01.lib.
      *
       fd arqimp

       label record is standard
       value of file-id is impress
       data record is reg-imp.

       01 reg-imp                      pic x(300).
      *
       working-storage section.
      *
       01 sl01-status                  pic x(02) value "00".
       01 sl01-stat                    pic x(01) value "F".
      *
       01 nome-arq-sl01.
          02 sl01-dir                  pic x(03) value "SL1".
          02 filler                    pic x(01) value "\".
          02 sl01-nome                 pic x(08) value "ARQSL01A".
          02 filler                    pic x(01) value ".".
          02 sl01-ext                  pic x(03) value "DAT".
      *
       01 impress                      pic x(12) value spaces.
       01 imp-status                   pic x(02) value "00".
       01 imp-stat                     pic x(01) value "F".
      *
       01 cb-prog.
          02 cb-programa               pic x(08) value "PGSL02".
          02 cb-versao                 pic x(06) value "v1.00 ".
      *
       01 limpa                        pic x(45) value spaces.
       01 limpa-aux                    pic x(15) value spaces.
       01 kbd-aux                      pic 9(02) comp-5 value 0.
       01 flag-fim                     pic x(01) value "N".
       01 campo-dorme                  pic 9(04) comp-5 value 3.
       01 chave-ant                    pic 9(05) value 0.
       01 tracos                       pic x(78) value all "-".
      *

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
           perform sec-limpa.
      *
       lab-fim.
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

      *
       rot-le-proximo.
           move 0 to erro.
           read arqsl01 next at end move 1 to erro.
           if sl01-status = "9D"
              move 0 to erro
              call "C_Wait" using by value campo-wait
              go to rot-le-proximo
           end-if.
      *
       rot-open-sl01.
           move 0 to erro.
           if sl01-stat = "F"
              open i-o arqsl01
              if sl01-status not = "00"
                 move 
                 " Erro de abertura no ARQSL01A.DAT - Tecle <Enter>" to 
                 mensagem
                 display tela-erro
                 perform rot-keypress
                 display tela-limpa
                 move 1 to erro
               else
                  move "A" to sl01-stat
               end-if
           end-if.
      *
       rot-close-sl01.
           if sl01-stat = "A"
              close arqsl01
              move "F" to sl01-stat
           end-if.
      *
       err-leitura-sl01.
           move " Erro de leitura - ARQSL01A.DAT - Tecle <Enter>" to
           mensagem.
           display tela-erro.
           perform rot-keypress.
           display tela-limpa.
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
       sec-limpa section.
      *
       lab-limpa-00.
           perform rot-open-sl01.
           if erro not = 0
              go to lab-limpa-fim
           end-if.
           move low-values to sl01-chave.
           start arqsl01 key is not less sl01-chave invalid key
                 perform err-leitura-sl01
                 go to lab-limpa-fim
           end-start.
      *
       lab-limpa-01.
           perform rot-le-proximo.
           if erro not = 0
              go to lab-limpa-fim
           end-if.
           if sl01-chave = high-values
              go to lab-limpa-fim
           end-if.
           move "N" to sl01-etiqueta.
           rewrite reg-sl01.
           start arqsl01 key is greater sl01-chave.
           go to lab-limpa-01.
      *
       lab-limpa-fim.
           perform rot-close-sl01.
           exit.