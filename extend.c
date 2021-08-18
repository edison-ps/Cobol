/*      Biblioteca de Funcoes para o COBOL 4.5     */
/*      v1.00         ??/03/93                     */
/*      v1.01         01/12/94                     */

#include <stdio.h>
#include <dos.h>

extern Save (unsigned int Ponteiro);

extern Restore (unsigned int Ponteiro, 
                unsigned int Carc_Atr);

/*------------------- Prototipos ------------------*/

void Gotoxy (unsigned char,
             unsigned char);

void Set_modo (unsigned char);

void Wait (unsigned int Temp);

void Writexy (unsigned char,
              unsigned char,
              unsigned char,
              unsigned char,
              unsigned char,
              unsigned char *);

void Savescr (unsigned char,
              unsigned char,
              unsigned char,
              unsigned char,
              unsigned int *);

void Restscr (unsigned char,
              unsigned char,
              unsigned char,
              unsigned char,
              unsigned int *);

void Box (unsigned char,
          unsigned char,
          unsigned char,
          unsigned char,
          unsigned char,
          unsigned char,
          unsigned char *);

long Kbdpress (void);

long Date (unsigned char *);

void Cls (void);

long Menu (unsigned char,
           unsigned char,
           unsigned char,
           unsigned char,
           unsigned char,
           unsigned char,
           unsigned char *);

/*-------------Funcoes nao Revisadas --------------*/

extern Dark  (unsigned int Ponteiro);

extern Scanc (void);

extern Cript (unsigned int Fator_n,
              unsigned char Fator_a,
              unsigned char Literal);

extern Decript (unsigned int Fator_n,
                unsigned char Fator_a,
                unsigned char Literal);

extern I_mouse (void);

extern D_mouse (void);

extern F_mouse (void);

extern L_mouse_b (void);

extern L_mouse_c (void);

extern L_mouse_l (void);

extern M_mouse (unsigned int Col,
                unsigned int lin);


/* Funcao para inicializar o mouse */

int Inic_mouse (void)

{
 
  return (I_mouse ());

}

/* Procedure para exibir o cursor do mouse */

void Disp_mouse (void)

{

  D_mouse ();

}

/* Procedure para ocultar o cursor do mouse */

void Fim_mouse (void)

{

  F_mouse ();

}

/* Procedure para mover o cursor do mouse */

void Move_mouse (unsigned char Col,
                 unsigned char Lin)

{

  unsigned int Coluna,
               Linha;

  Coluna = 8 * Col;
  Linha = 8 * Lin;
  M_mouse (Coluna, Linha);

}

/* Produz uma sombra na tela */

void Darkscr (unsigned char Col,
              unsigned char Lin)

{

  unsigned int  Ponteiro;

  Ponteiro = Col * 2 + (Lin * 160);
  Dark (Ponteiro);

}


/* Procedure para aguadar N segundos */

void Wait (unsigned int Temp)

{

  double Seg;

  if (Temp == 0) 

     Temp = 1;

  for (Seg = 0; Seg <= Temp * 1000; Seg++) {

  }

}

/* Funcao para encriptar/desencriptar textos */

void Criptxt (unsigned int Opcao,
              unsigned int Tam,
              unsigned char *Tabela)

{

  unsigned int Sub;

  for (Sub = 0; Sub <= Tam + 1; Sub++) {

      switch (Opcao) {

            case 1 : 

                 *(Tabela + Sub + 1) = Cript ((Tam - Sub) + 1, *Tabela - Sub, *(Tabela + Sub + 1));
                 break;
            
            case 2 : 

                 *(Tabela + Sub + 1) = Decript ((Tam - Sub) + 1, *Tabela - Sub, *(Tabela + Sub + 1));
                 break;
      }

  }

}

/* Funcao para trasformar um texto caracteres alfanumericos maiusculos */

void Text (unsigned char *Tabela)

{

  unsigned int Sub,
               Sub_aux;

  unsigned char Aux,
               Tabela_aux [80];

  Sub = Sub_aux = 0;
  
  do {

     Aux = 'X';
     *(Tabela + Sub) = toupper (*(Tabela + Sub));

     if ((*(Tabela + Sub) == ' ' ) || (*(Tabela + Sub) >= 48 && *(Tabela + Sub) <= 57 )
        || (*(Tabela + Sub) >= 65 && *(Tabela + Sub) <= 90)) 

        Aux = ' ';

     if (Aux != ' ') 

        *(Tabela + Sub) = ' ';

     Sub++;

  } while (*(Tabela + Sub) != 0);

  Sub = Sub_aux = 0;

  do {

     Tabela_aux [Sub] = ' ';

     if (*(Tabela + Sub) != ' ') {

        Tabela_aux [Sub_aux] = *(Tabela + Sub);
        *(Tabela + Sub) = ' ';
        Sub_aux++;

     }

     else

        if (*(Tabela + Sub + 1) != ' ' && *(Tabela + Sub + 2) != ' ') {

           Tabela_aux [Sub_aux] = *(Tabela + Sub);
           *(Tabela + Sub) = ' ';
           Sub_aux++;

        }

    Sub++;

  } while (*(Tabela + Sub) != 0);

  Sub_aux = 0;
  
  do {

     *(Tabela + Sub_aux) = Tabela_aux [Sub_aux];
     Sub_aux++;

  } while (Sub_aux < Sub);

}



/* Funcao para retornar uma tecla caso seja pressionada alguma */

long Readkey (void)

{

  int Tecla = 0,
      Col_m,
      Mouse,
      Lin_m;

  Mouse = L_mouse_b ();
  Col_m = L_mouse_c () / 8;
  Lin_m = L_mouse_l () / 8; 

  if (Mouse == 1 || Mouse == 2) {

     switch (Mouse) {

            case 1 : {

                 Mouse = (Col_m * 2 + (Lin_m * 160)) * -1;
                 break;

            }

            case 2 : {

                 Mouse = -1;
                 break;
           
            }

     }

     return (Mouse);
  }

  else {

       Gotoxy (80, 25);
       if (kbhit () != 0) 

       Tecla = Scanc ();

       return (Tecla);

  }

}

/*---------------Funcoes Revisadas ----------------*/

/* Procedure para dar display de strings */


void Writexy (unsigned char Col,
              unsigned char Lin,
              unsigned char Tam,
              unsigned char Cor_f,
              unsigned char Cor_p,
              unsigned char *Literal)
{

  unsigned int Sub,
               Cor;

  char far *Video = (char far *) 0xb8000000;


  if (Tam == 0) 
 
     Tam = 80;

  Video += Col * 2 + (Lin * 160);

  Cor_f = Cor_f << 4; 
  Cor = Cor_p | Cor_f;
  
  for (Sub = 0; Sub <= Tam - 1; Sub++) {

      *Video = *(Literal + Sub);
      Video ++;
      *Video = (char) Cor;
      Video ++;

  }          

}

/* Procedure para abrir janelas */

void Box (unsigned char Col_i,
          unsigned char Lin_i,
          unsigned char Col_f,
          unsigned char Lin_f,
          unsigned char Cor_f,
          unsigned char Cor_p,
          unsigned char Atr[3])

/*        Atr = 0 - Borda
          Atr = 1 - Fundo
          Atr = 2 - Sombra       */

{


  unsigned char  X,
                 Y,
                 B1,
                 B2,
                 B3,
                 B4,
                 Col,
                 Lin,
                 Fundo;

  switch (Atr [0]) {

         case '1' :

              B1 = 218;
              B2 = 191;
              B3 = 217;
              B4 = 192;
              Col = 179;
              Lin = 196;
              break;

         case '2' :
              
              B1 = 201;
              B2 = 187;
              B3 = 188;
              B4 = 200;
              Col = 186;
              Lin = 205;
              break;

         case '3' :
              
              B1 = 213;
              B2 = 184;
              B3 = 190;
              B4 = 212;
              Col = 179;
              Lin = 205;
              break;

         default:

              B1 = Atr [0];
              B2 = Atr [0];
              B3 = Atr [0];
              B4 = Atr [0];
              Col = Atr [0];
              Lin = Atr [0];

  }


  Writexy (Col_i, Lin_i, 1, Cor_f, Cor_p, &B1);

  for (X = Col_i + 1; X <= Col_f - 1; X++) {

      Writexy (X, Lin_i, 1, Cor_f, Cor_p, &Lin);
  }
  
 Writexy (Col_f, Lin_i, 1, Cor_f, Cor_p, &B2);
 Y = Lin_i;

 do {
  
    Y++;
    Writexy (Col_i, Y, 1, Cor_f, Cor_p, &Col);

    for (X = Col_i + 1; X <= Col_f - 1; X++) {

        Writexy (X, Y, 1, Cor_f, Cor_p, &Atr [1]);
    }

    Writexy (Col_f, Y, 1, Cor_f, Cor_p, &Col);

    if (Atr [2] == 'S') {

       Darkscr (X + 1, Y);
       Darkscr (X + 2, Y);

    }

  } while (Y <= Lin_f - 1);

  Writexy (Col_i, Lin_f, 1, Cor_f, Cor_p, &B4);

  for (X = Col_i + 1; X <= Col_f - 1; X++) {

      Writexy (X, Lin_f, 1, Cor_f, Cor_p, &Lin);

  }

  Writexy (Col_f, Lin_f, 1, Cor_f, Cor_p, &B3);

  if (Atr [2] == 'S') {

     Darkscr (Col_f + 2, Lin_f);  
     
     for (X = Col_i + 2; X <= Col_f + 2; X++) {

         Darkscr (X, Lin_f + 1);

     }

  }

}

/* Funcao para salvar a tela */

/* void Savescr (unsigned char Col_i,
              unsigned char Lin_i,
              unsigned char Col_f,
              unsigned char Lin_f,
              unsigned int *Buffer)

{

  int far *Video = (int far *) 0xb8000000;

  unsigned char  X, 
                 Y;

  unsigned int Sub = 0;

  for (Y = Lin_i; Y <= Lin_f; Y++) {

      for (X = Col_i; X <= Col_f; X++) {

          *(Buffer + Sub) = *(Video + X +(Y * 80));
          Sub++;
          
      }

  } 

}*/

/* Procedure para restaurar a tela */

/*void Restscr (unsigned char Col_i,
              unsigned char Lin_i,
              unsigned char Col_f,
              unsigned char Lin_f,
              unsigned int *Buffer)

{
  int far *Video = (int far *) 0xb8000000;

  unsigned char  X, 
                 Y;

  unsigned int Sub = 0;

  for (Y = Lin_i; Y <= Lin_f; Y++) {

      for (X = Col_i; X <= Col_f; X++) {

          *(Video + X + (Y * 80)) = *(Buffer + Sub);
          Sub++;
          
      }

  } 

}*/

void Savescr (unsigned char Col_i,
              unsigned char Lin_i,
              unsigned char Col_f,
              unsigned char Lin_f,
              unsigned int *Buffer)

{

  unsigned int   Ponteiro,
                 X, 
                 Y,
                 Sub = 0;


  Y = Lin_i;

  do {
  
     for (X = Col_i; X <= Col_f; X++) {

         Ponteiro = X * 2 + (Y * 160);
         *(Buffer + Sub) = Save (Ponteiro);
         Sub++;

     }

     Y++;  

  } while (Y < Lin_f + 1);

}

/* Procedure para restaurar a tela */

void Restscr (unsigned char Col_i,
              unsigned char Lin_i,
              unsigned char Col_f,
              unsigned char Lin_f,
              unsigned int *Buffer)

{

  unsigned int   Ponteiro,
                 X, 
                 Y,
                 Sub = 0;

  Y = Lin_i;

  do {
  
     for (X = Col_i; X <= Col_f; X++) {

         Ponteiro = X * 2 + (Y * 160);
         Restore (Ponteiro, *(Buffer + Sub));
         Sub++;

     }

     Y++;  

  } while (Y < Lin_f + 1);

}


/*  Procedure para posicionar o cursor  */

void Gotoxy (unsigned char Col,
             unsigned char Lin)

{

  union REGS regs;

  regs.h.ah = 0x2;   
  regs.h.bh = 0;
  regs.h.dh = Lin;
  regs.h.dl = Col;
  int86 (0x10, &regs, &regs);

}

/*  Procedure para selecionar modo de video */

void Set_modo (unsigned char Modo)

{
  union REGS regs;
   
  regs.h.ah = 0;
  regs.h.al = Modo;
  int86 (0x10, &regs, &regs);


}

/* Funcao para verificar se alguma tecla foi pressionada */

long Kbdpress (void)

{

  Gotoxy (80, 25);
  return (kbhit());

}

/* Procedure para apagar a tela */

void Cls (void)

{

  int far *Video = (int far *) 0xb8000000;

  int Sub;

  for (Sub = 0; Sub <= 2000; Sub++) {

      *(Video + Sub) = 0x0720;

  } 

}

/* Funcao para fazer menu de barra */

long Menu (unsigned char Argum,
           unsigned char Tam,
           unsigned char Cor_f,
           unsigned char Cor_p,
           unsigned char Cor_fb,
           unsigned char Cor_pb,
           unsigned char *Tabela_c)

{

  union Alfa_num {

        unsigned int Num;
        unsigned char Alfa [2];

  } Uniao;


  unsigned int Tecla,
               Buffer [40];

  long Teclado;

  int Sub,
      St_mouse,
      X;

  unsigned char Tab_aux [21],
                Tab_sub [21],
                Col_a [21],
                Lin_a [21],
                Mouse,
                Col_m,
                Lin_m,
                Zero,
                Ativo,
                Y,
                Z;

  for (Zero = 0; ; Zero++) {

      if (*(Tabela_c + Zero) == 179) break;

  }

  Zero++; 
  Tam += 2;

  for (Sub = 0; Sub <= Argum; Sub++) {

      *(Col_a + Sub) = *(Tabela_c + (Sub * Tam + Zero));
      *(Lin_a + Sub) = *(Tabela_c + (Sub * Tam + Zero + 1));

      Writexy (*(Col_a + Sub), *(Lin_a + Sub), (Tam - 2), Cor_f, Cor_p, Tabela_c + (Sub * Tam + Zero + 2));

  }

  for (Sub = 0; Sub <= Argum; Sub++) {

      for (X = 0; X <= (Tam - 2); X++) {

          if (*(Tabela_c + (Sub * Tam + Zero + X + 2)) != ' ') {

             *(Tab_aux +Sub) = *(Tabela_c + (Sub * Tam + Zero + X + 2));
             *(Tab_sub + Sub) = X;
             break;

          }

      }

  }

/*  Writexy (0, 20, 402, 2, 7, &Tabela_c [Zero - 2]); */

  X = 0;
  Tecla = 0;
  Sub = Tabela_c [Zero - 2];
  Savescr (*(Col_a + Sub), *(Lin_a + Sub), *(Col_a + Sub) + (Tam - 2), *(Lin_a + Sub), Buffer);
  St_mouse = Inic_mouse (); 
  Move_mouse (*(Col_a + Sub) + *(Tab_sub + Sub), *(Lin_a + Sub)); 

  do {

     Writexy (*(Col_a + Sub), *(Lin_a + Sub), (Tam - 2), Cor_fb, Cor_pb, Tabela_c + (Sub * Tam + Zero + 2));

     if (St_mouse != 0) Disp_mouse (); 
     Gotoxy (80, 25);
     Tecla = 20;
     Uniao.Num = Ativo = 0;

     do {

        Ativo = 0;
        Teclado = kbdpress ();
        Mouse = L_mouse_b ();
        Col_m = L_mouse_c () / 8;
        Lin_m = L_mouse_l () / 8;
        
        if (Teclado != 0) {

           Uniao.Num = Scanc ();
           Tecla = Uniao.Alfa [0];
           Ativo = 1;
 
        }

        if (Mouse != 0) {

           switch (Mouse) {

                  case 1 : {

                       for (Z = 0; Z <= (Tam - 2); Z++) {

                           if (Lin_m == *(Lin_a + Z)) {

                              if (Col_m >= *(Col_a + Z) && Col_m <= *(Col_a + Z) + (Tam - 2)) {

                                 Uniao.Alfa [0] = 0;
                                 Uniao.Alfa [1] = toupper(*(Tab_aux +Z));
                                 Teclado = kbdpress ();
                                 Ativo = 1;

                              }

                           }

                       }

                       break;

                  }

                  case 2 : 

                       Tecla = 1;
                       Ativo = 1;
                       Teclado = kbdpress ();
                       break;

           }

        }

     } while (Ativo == 0);

     Fim_mouse (); 
  
     switch (Tabela_c [Zero - 3]) {

             case 'V' :
  
                  if (Tecla == 72) {

                     Restscr (*(Col_a + Sub), *(Lin_a + Sub), *(Col_a + Sub) + (Tam - 2), *(Lin_a + Sub), Buffer);
                     Sub --;
                     Tecla = 0;

                  }
                 
                  if (Tecla == 80) {

                     Restscr (*(Col_a + Sub), *(Lin_a + Sub), *(Col_a + Sub) + (Tam - 2), *(Lin_a + Sub), Buffer);
                     Sub ++;
                     Tecla = 0;

                  }

                  break;

             case 'H' : 
  
                  if (Tecla == 75) {

                     Restscr (*(Col_a + Sub), *(Lin_a + Sub), *(Col_a + Sub) + (Tam - 2), *(Lin_a + Sub), Buffer);
                     Sub --;
                     Tecla = 0;

                  }
                 
                  if (Tecla == 77) {

                     Restscr (*(Col_a + Sub), *(Lin_a + Sub), *(Col_a + Sub) + (Tam - 2), *(Lin_a + Sub), Buffer);
                     Sub ++;
                     Tecla = 0;

                  }

                  break;

     }

     if (Tecla != 0 && Tecla != 28) {

        Restscr (*(Col_a + Sub), *(Lin_a + Sub), *(Col_a + Sub) + (Tam - 2), *(Lin_a + Sub), Buffer);

     }

     for (Y = 0; Y <= Argum; Y++) {

         if (toupper(*(Tab_aux + Y)) == toupper (Uniao.Alfa [1])) {

            Restscr (*(Col_a + Sub), *(Lin_a + Sub), *(Col_a + Sub) + (Tam - 2), *(Lin_a + Sub), Buffer);
            Sub = Y;
            X = -3;
            Writexy (*(Col_a + Sub), *(Lin_a + Sub), (Tam - 2), Cor_fb, Cor_pb, Tabela_c + (Sub * Tam + Zero + 2));
            break;

         }

     }

     Tabela_c [Zero - 2] = Sub;

     if (X == 0) {

        if (Tecla == 1) X = -2;
        if (Tecla == 28) X = -3;
        if (Sub < 0 ) Sub = Argum - 1;
        if (Sub > Argum - 1) Sub = 0; 
        Savescr (*(Col_a + Sub), *(Lin_a + Sub), *(Col_a + Sub) + (Tam - 2), *(Lin_a + Sub), Buffer);

     }

  } while (X == 0);

  if (X == -3) X = Sub;
  return (X + 1);

}

/*---------------------------------------------------------------------------*/
/* Funcoes para consistencia de data                                         */
/*---------------------------------------------------------------------------*/

void Norm_euro (unsigned char *, 
                unsigned char *);

void Euro_norm (unsigned char *, 
                unsigned char *);

void Norm_inv (unsigned char *,
               unsigned char *);

void Inv_norm (unsigned char *,
               unsigned char *);

unsigned int Euro_dias (unsigned char *);

void Dias_euro (unsigned int,
                unsigned char *);

/* Funcao para consistencia de data */

long Date (unsigned char *Datas)

{

  unsigned int Sub,
               Dia,
               Mes,
               Ano,
               Erro;              

  unsigned char Opcao_data,
                Data_norm [3],
                Data_inv [3],
                Data_euro [5];

  float Resto;

  union Dias_c {

        unsigned char Dias_a [2];
        unsigned int Dias_n;

  } Dias_corr;

  Erro = 0;
  Opcao_data = *Datas;
  Dias_corr.Dias_a [0] = *(Datas + 1);
  Dias_corr.Dias_a [1] = *(Datas + 2);
  Data_norm [0] = *(Datas + 3);
  Data_norm [1] = *(Datas + 4);
  Data_norm [2] = *(Datas + 5);
  Data_inv [0] = *(Datas + 6);
  Data_inv [1] = *(Datas + 7);
  Data_inv [2] = *(Datas + 8);
  Data_euro [0] = *(Datas + 9);
  Data_euro [1] = *(Datas + 10);
  Data_euro [2] = *(Datas + 11);
  Data_euro [3] = *(Datas + 12);
  Data_euro [4] = *(Datas + 13);

  switch (Opcao_data) {

         case '1' : 

              Dias_euro (Dias_corr.Dias_n, Data_euro);
              Euro_norm (Data_euro, Data_norm);
              Norm_inv  (Data_norm, Data_inv);
              break;

         case '2' :

              Norm_inv (Data_norm, Data_inv);
              Norm_euro (Data_norm, Data_euro);
              Dias_corr.Dias_n = Euro_dias (Data_euro);
              break;

         case '3' :

              Inv_norm (Data_inv, Data_norm); 
              Norm_euro (Data_norm, Data_euro);
              Dias_corr.Dias_n = Euro_dias (Data_euro);
              break;

         case '4' :

              Resto = Data_euro [4] % 4;

              if (Resto == 0) 

                 Dia = 29;
 
              else

                 Dia = 28;

              if (Data_euro [2] < 1 || Data_euro [2] > 12) {

                 Erro = 1;
                 break;

              }

              if ((Data_euro [2] == 1 || Data_euro [2] == 3 || Data_euro [2] == 5
                 || Data_euro [2] == 7 || Data_euro [2] == 8 || Data_euro [2] == 10 
                 || Data_euro [2] == 12) && (Data_euro [0] < 1 || Data_euro [0] > 31)) {

                 Erro = 1;
                 break;

              }
     
              if ((Data_euro [2] == 4 || Data_euro [2] == 6 || Data_euro [2] == 9 
                 || Data_euro [2] == 11) && (Data_euro [0] < 1 || Data_euro [0] > 30)) {

                 Erro = 1;
                 break;

              }

              if (Data_euro [2] == 2 && (Data_euro [0] < 1 || Data_euro [0] > Dia)) {

                 Erro = 1;
                 break;

              }

              Euro_norm (Data_euro, Data_norm);
              Dias_corr.Dias_n = Euro_dias (Data_euro);
              Norm_inv  (Data_norm, Data_inv);
              break;

          default:

              Erro = 1;

  }

  if (Erro == 0) {

     *(Datas + 1) = Dias_corr.Dias_a [0];
     *(Datas + 2) = Dias_corr.Dias_a [1];
     *(Datas + 3) = Data_norm [0];
     *(Datas + 4) = Data_norm [1];
     *(Datas + 5) = Data_norm [2];
     *(Datas + 6) = Data_inv [0];
     *(Datas + 7) = Data_inv [1];
     *(Datas + 8) = Data_inv [2];
     *(Datas + 9) = Data_euro [0];
     *(Datas + 10) = Data_euro [1];
     *(Datas + 11) = Data_euro [2];
     *(Datas + 12) = Data_euro [3];
     *(Datas + 13) = Data_euro [4]; 

  }

  return (Erro);

}

void Norm_euro (unsigned char Data_norm [3], 
                unsigned char Data_euro [5])

{

  Data_euro [0] = Data_norm [0];
  Data_euro [1] = '/';
  Data_euro [2] = Data_norm [1];
  Data_euro [3] = '/';
  Data_euro [4] = Data_norm [2];

}

void Euro_norm (unsigned char Data_euro [5], 
                unsigned char Data_norm [3])

{

  Data_norm [0] = Data_euro [0];
  Data_norm [1] = Data_euro [1];
  Data_norm [2] = Data_euro [2];

}

void Norm_inv (unsigned char Data_norm [3],
               unsigned char Data_inv [3])

{

  Data_inv [0] = Data_norm [2];
  Data_inv [1] = Data_norm [1];
  Data_inv [2] = Data_norm [0];
  
}

void Inv_norm (unsigned char Data_inv [3],
               unsigned char Data_norm [3])

{

  Data_norm [0] = Data_inv [2];
  Data_norm [1] = Data_inv [1];
  Data_norm [2] = Data_inv [0];
  
}

unsigned int Euro_dias (unsigned char Data_euro [5])

{

  double Resto,
         Fracao,
         Bissextos;

  unsigned int Anos,
               Meses,
               Inteiro,
               Aux,
               Dias,
               Sub;

  unsigned char Tab_meses [12];

  Tab_meses [0] = 31;
  Tab_meses [1] = 28;
  Tab_meses [2] = 31;
  Tab_meses [3] = 30;
  Tab_meses [4] = 31;
  Tab_meses [5] = 30;
  Tab_meses [6] = 31;
  Tab_meses [7] = 31;
  Tab_meses [8] = 30;
  Tab_meses [9] = 31;
  Tab_meses [10] = 30;
  Tab_meses [11] = 31;

  if (Data_euro [4] < 12)

     Anos = 2000 + Data_euro [4];

  else

     Anos = 1900 + Data_euro [4];

  Anos = Anos - 1912;
  Dias = Anos * 365;
  Bissextos =  Anos / 4;
  Fracao = Bissextos;
  Bissextos = Inteiro = Bissextos;
  Resto = Anos % 4;

  if (Resto == 0 && Fracao - Inteiro == 0) 

     Tab_meses [1] = 29;

  else

     Bissextos++;

  Resto = 0;
                
  for (Sub = 0; Sub < Data_euro [2] - 1; Sub++) {

      Resto = Resto + Tab_meses [Sub];

  }

  return (Dias + Bissextos + Resto + Data_euro [0]);

}

void Dias_euro (unsigned int Dias_corr,
                unsigned char Data_euro[5])

{

  double Resto,
         Fracao,
         Bissextos;

  unsigned int Sub,
               Inteiro,
               Aux;

  unsigned char Dias,
                Meses,
                Anos,
                Tab_meses [12];

  Tab_meses [0] = 31;
  Tab_meses [1] = 28;
  Tab_meses [2] = 31;
  Tab_meses [3] = 30;
  Tab_meses [4] = 31;
  Tab_meses [5] = 30;
  Tab_meses [6] = 31;
  Tab_meses [7] = 31;
  Tab_meses [8] = 30;
  Tab_meses [9] = 31;
  Tab_meses [10] = 30;
  Tab_meses [11] = 31;

  Anos = Dias_corr / 365;
  Bissextos = Anos / 4;
  Fracao = Bissextos;
  Bissextos = Inteiro = Bissextos;
  Resto = Anos % 4;

  if (Resto == 0 && Fracao - Inteiro == 0) 

     Tab_meses [1] = 29;

  else

     Bissextos++;

  Dias_corr = Dias_corr - Bissextos;
  Anos = Dias_corr / 365;
  Resto = Anos % 4;

  if (Resto != 0) 

     Tab_meses [1] = 28;

  Resto = Dias_corr % 365;
  Resto = Inteiro = Resto;
  Aux = Sub = 0;

  while (Aux + Tab_meses [Sub] <= Resto && Sub < 12) {

        Aux = Aux + Tab_meses [Sub];
        Sub++;

  }

  Meses = Sub + 1;

  if (Aux == Resto) {

     if (Resto == 0) {

        Dias = 31;
        Meses = 12;
        Anos--;

     }

     else {

        Meses--;
        Dias = Tab_meses [Sub - 1];

     }

  }
  
  else

     Dias = Resto - Aux;

  if (Anos >= 88) 

     Anos = Anos - 88;

  else

     Anos = Anos + 12;

  Data_euro [0] = Dias;
  Data_euro [1] = '/';
  Data_euro [2] = Meses;
  Data_euro [3] = '/';
  Data_euro [4] = Anos;

}
