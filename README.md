FenomensColectius i Transicions de Fase 2
==========================================

Pràctiques de **Fenòmens Col·lectius i Transicions de Fase**, *Física,
Universitat de Barcelona*.

**Ising 2D Monte Carlo** simulation using the *Metropolis Algorithm*.

Random number generator: *RCARIN & RCARRY* by *F. James* (1991 version)
using the *Marsaglia-Zaman Algorithm*.

`scriptMC2.sh` compila `MC2.f`, executa `MC2.exe` per `L=8,16,32,64,128`, i
genera les gràfiques respecte la temperatura corresponents a $\langle e \rangle$,
$\langle |m| \rangle$ i $\sqrt{m^2}$, $c_{v}$ i $\frac{d \langle e\rangle}{dT}$, $\chi$ per les diferents `L`.



`MC2.exe` necessita un fitxer de dades inicial amb el format:

  ```
  &DADES  
  NOM="MC-L<xxx>-T<yyyy>-TPAS<zzz>-TINC<www>",
  L=<xxx>,      Costat de la xaxa  
  TEMP=<yyyy>,  Temperatura inicial  
  TPAS=<zzz>,   Nombre de temperatures a calcular  
  TINC=<www>,   Increment de temperatura  
  NLLAV=200,    Nombre de llavors a utilitzar per cada temperatura  
  LLAV0=156,    Llavor inicial  
  MCTOT=12000,  Nombre total de MC a realitzar per cada llavor  
  MCINI=2000,   Els MCINI calculs MC inicials es descartaran  
  MCD=10        Passos de MC entre enregistraments de dades
  &END
  ```
