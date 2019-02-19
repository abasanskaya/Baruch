TITLE 'CHECK FOR NORMALITY AND OUTLIERS';
PROC IML;
  USE Project.ANIMAL1;
  READ ALL VAR{T Tel G_CON C_CON T_CON A_CON  Log_AWeight Log_MRate Log_Max_longevity} INTO X;
  N = NROW(X);
  P = NCOL(X);
  XBAR = 1/N*X`*J(N,1);
  XBARM = REPEAT(XBAR,1,N);                  /* NxN MATRIX OF XBAR VALUES */
  S = 1/(N-1)*X`*(I(N)-1/N*J(N))*X;
  D = (X`-XBARM)`*INV(S)*(X`-XBARM);
  Di2 = VECDIAG(D);
  SIGHAT = 1/N*X`*(I(N)-1/N*J(N))*X;
  G = (X`-XBARM)`*INV(SIGHAT)*(X`-XBARM);
  b1p = SUM(G##3)/N##2;
  b2p = TRACE(G##2)/N;
  PRINT Di2, b1p, b2p;

  U = N*Di2/(N-1)##2;
  create Usort from U;
  append from U;
  close Usort;
  sort Usort by col1;
  use usort;
  read all var{col1} into U;
  alpha = (p-2)/(2*p);
  beta = (n-p-3)/(2*(n-p-1));
  a = p/2;
  b = (n-p-1)/2;
  V = J(n,1);
  do i = 1 to n;
     prob = (i-alpha)/(n-alpha-beta+1);
     V[i] = betainv(prob,a,b);
  end;
  plotpts = V||U;
  colnme = {"vi","ui"};
  create QQ from plotpts[colname=colnme];
  append from plotpts; /*[colname=colnme]*/
quit;