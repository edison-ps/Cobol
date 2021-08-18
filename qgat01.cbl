      ***************************************************************
      *                                                             *
      *  A B A V / C N - I N F O R M A T I C A    :::  QGAT01       *
      *                                                             * 
      *-------------------------------------------------------------*
      *                                                             *
      *  Data da ultima alteracao:    11/11/94     v1.00            *
      *                                                             *
      ***************************************************************
      * 
       identification division.
       program-id. qgat01.
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
           select arqat99 assign to disk
                  organization is indexed
                  access mode is dynamic
                  lock mode is manual
                  with lock on multiple records
                  record key is at99-chave
                  alternate record key is at99-chave-1 with duplicates
                  alternate record key is at99-chave-2 with duplicates
                  file status is at99-status.
      *
       data division.
       file section.
      *    
       copy fdat01.lib.
      *    
       copy fdat99.lib.
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
       01 at99-status                  pic x(02) value "00".
       01 at99-stat                    pic x(01) value "F".
      *
       01 nome-arq-at99.
          02 at99-dir                  pic x(04) value "\TMP".
          02 filler                    pic x(01) value "\".
          02 at99-nome                 pic x(08) value "ARQAT99A".
          02 filler                    pic x(01) value ".".
          02 at99-ext                  pic x(03) value "DAT".
      *
       01 cb-programa                  pic x(01) value spaces.
       01 cb-versao                    pic x(01) value spaces.
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
           open i-o arqat01 arqat99.
           move low-values to at99-chave.
           start arqat99 key is not less at99-chave.
      *
       lab-01.
           read arqat99 next at end go to lab-fim.
           if at01-chave  = high-values
              go to lab-01
           end-if.
           move reg-at99 to reg-at01.
           write reg-at01 invalid key
                 display at01-chave " <----- erro"
                 stop " "
           end-write.
           display at01-chave.
           go to lab-01.
      *
       lab-fim.
           close arqat01 arqat99.
           stop run.