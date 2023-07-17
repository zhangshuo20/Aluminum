$title Epsilon Constraint Method for aluminum Model

$onText
The eps-Constraint Method

Additional information can be found at:

http://www.gams.com/modlib/adddocs/epscm.pdf

Mavrotas, G, Effective implementation of the eps-constraint method in
Multi-Objective Mathematical Programming problems.
Applied Mathematics and Computation 213, 2 (2009), 455-465.

Keywords: linear programming, eps-constraint method, multiobjective optimization
$offText

$if %system.filesys% == UNIX $abort.noerror 'This model cannot run on a non-Windows platform';
$call msappavail -Excel
$if errorlevel 1 $abort.noerror 'Microsoft Excel is not available!';

$inlineCom [ ]
$eolCom //

$stitle  Model Definitions
Set
   s 'supplier'              / Al_supplier_1,  Al_supplier_2, Al_supplier_3 /
   f 'plant'                 / plant_1, plant_2, plant_3 /
   w 'warehouse'             / warehouse_1, warehouse_2 /
   c 'customer'              / customer_1, customer_2, customer_3/
   t 'transport mode'        / road,  rail, barge /
   p 'product'               / product_A, product_B /
   r 'raw material type'     / Al /
   k 'objective functions'   / cost, water /
   ;

$set min -1
$set max +1

Parameter dir(k) 'direction of the objective functions'
                 / cost %min%, water %min% /;

*$if not set cp $set cp 30
Scalar cp        'carbon price (yuan)'                                             / 30 /
       lifetime  'lifetime'                                                        / 25 /
       ir        'interest rate'                                                   / 0.06 /
    ;


Parameter

   ssel(s)        'supplier selection cost'

   scap(s,r)      'capacity (ton) for raw material r of supplier s'

   sem(s,r)       'CO2 emissions(ton) for puducting one ton raw material r in supplier s'

   sprc(s,r)      'price (yuan) of one ton raw material r for supplier s'

   swater(s,r)    'water consumed (ton) for making one ton raw material r for supplier s'

   cr(f,r)        'conversion rate from feedstock to product'

   fcap(f)        'total capacity (ton) in plant f'

   ftc(f)         'total fixed costs (yuan) of establishing plant f'

   fce(f)         'annual fixed costs (yuan) of establishing plant f'

   pcep(p)        'variable costs(yuan) of producing one ton of product p'

   pwater(p)      'water(ton) of producing one ton of producr p'

   wcap(w)        'total capacity (ton) in warehouse w'

   wtc(w)         'total fixed costs (yuan) of establishing warehouse w'

   wce(w)         'annual fixed costs (yuan) of establishing warehouse w'

   pup(p)         'price (yuan) per ton of product p'

   pemp(p)        'CO2 emissions(ton) for producing one ton of product p'

   sfcost(s,f,t)  'shipping cost (yuan per km per ton) from supplier s to plant f in transport mode t'

   sfdis(s,f,t)   'distance (km) from supplier s to plant f in transport mode t'

   fwcost(f,w,t)  'shipping cost (yuan per km per ton) from  plant f  to warehouse w in transport mode t'

   fwdis(f,w,t)   'distance (km)  from  plant f  to warehouse w in transport mode t'

   wccost(w,c,t)  'shipping cost (yuan per km per ton) from warehouse w to customer c in transport mode t'

   wcdis(w,c,t)   'distance (km) from warehouse w to customer c in transport mode t'

   tem(t)         'CO2 emissions(ton/(ton,km) in transport mode t'

   dmcp(c,p)      'demand (tons) of product p of customer c'
;

$CALL GDXXRW ../data/aluminum.xlsx Index=Index!A1:E23 trace=0
$GDXIN aluminum.gdx
$LOAD ssel=ssel
$LOAD scap=scap
$LOAD sem=sem
$LOAD swater=swater
$LOAD sprc=sprc
$LOAD cr=cr
$LOAD fcap=fcap
$LOAD ftc=ftc
$LOAD pcep=pcep
$LOAD pwater=pwater
$LOAD wcap=wcap
$LOAD wtc=wtc
$LOAD pup=pup
$LOAD pemp=pemp
$LOAD sfcost=sfcost
$LOAD sfdis=sfdis
$LOAD fwcost=fwcost
$LOAD fwdis=fwdis
$LOAD wccost=wccost
$LOAD wcdis=wcdis
$LOAD tem=tem
$LOAD dmcp=dmcp

$GDXIN
;

fce(f) = ftc(f)*(ir/((1-(1+ir)**(0-lifetime))));
wce(w) = wtc(w)*(ir/((1-(1+ir)**(0-lifetime))));




Variable
   z(k)         'objective function variables';


positive variable

VRP(s,f,r)      'amount of raw material r (tons) purchased from supplier s in plant f'

VRSF(s,f,t,r)   'amount of raw material r (tons) shipped from supplier s to  plant f with transport mode t'

VPFW(p,f,w,t)   'amount of product p (tons) shipped from plant f to warehouse w with transport mode t'

VPWC(p,w,c,t)   'amount of product p (tons) shipped from warehouse w  to customer c with transport mode t'

VPS(p,f)        'amount of product p (tons) produced from plant f'

;

binary variable

VSB(s) '1 if supplier f is established 0 otherwise'

VFB(f) '1 if plant f is established 0 otherwise'

VWB(w) '1 if warehouse w is established, 0 otherwise'
;


variable

z      'overall costs'

wt     'water consumed'

pc     'purchasing cost of raw materials'

mc     'manufacturing and storage cost'

tc     'transportation cost'

pco2   'amount of co2 in purchasing process'

mco2   'amount of co2 in manufacturing process'

tco2   'amount of co2 in transportation process'

co2    'total co2'

;


Equation
   objcost             'objective for minimizing cost'
   objwater            'objective for minimizing water consumption'

   purchasingcost      'purchasing cost of raw materials'
   manufacturingcost   'manufacturing and storage cost'
   transportationcost  'transportation cost'
   purchasingco2       'amount of co2 in purchasing process'
   manufacturingco2    'amount of co2 in manufacturing process'
   transportationco2   'amount of co2 in transportation process'
   totalco2                'total co2 equ'
   row1(s,r)           'supplier capacity constraints'
   row2(f,r)           'amounts of raw material r saticify production'
   row3(f)             'plant capacity constraints'
   row5(p,w)           'inflow of products to warehouse equal to outflow '
   row6(c,p)           'amounts of products transported from warehouse to customers equal to their demands'
   row7(w)             'warehouse capacity constraints'
   row8(s,f,r)         'feedstock transport to plant'
   row9(f,p)           'amounts of products of plant equals to outflow'
;

purchasingcost .. pc =e= sum(s,ssel(s)*VSB(s))
                         +cp*sum((s,f,r), sem(s,r)*VRP(s,f,r))
                         +sum((s,f,r), sprc(s,r)*VRP(s,f,r));

manufacturingcost .. mc =e=   sum(f, fce(f)*VFB(f))
                             +sum(w, wce(w)*VWB(w))
                             +sum((p,f), pcep(p)*VPS(p,f))
                             +cp*sum((p,f), pemp(p)*VPS(p,f));

transportationcost.. tc =e=  sum((s,f,t,r),sfdis(s,f,t)*sfcost(s,f,t)*VRSF(s,f,t,r))
                             +cp*sum((s,f,t,r),tem(t)*sfdis(s,f,t)*VRSF(s,f,t,r))
                             +sum((p,f,w,t),fwcost(f,w,t)*fwdis(f,w,t)*VPFW(p,f,w,t))
                             +cp*sum((p,f,w,t),tem(t)*fwdis(f,w,t)*VPFW(p,f,w,t))
                             +sum((p,w,c,t),wccost(w,c,t)*wcdis(w,c,t)*VPWC(p,w,c,t))
                             +cp*sum((p,w,c,t),tem(t)*wcdis(w,c,t)*VPWC(p,w,c,t));


purchasingco2..      pco2 =e= sum((s,f,r), sem(s,r)*VRP(s,f,r));

manufacturingco2..   mco2 =e= sum((p,f), pemp(p)*VPS(p,f));

transportationco2..  tco2 =e=  sum((s,f,t,r),tem(t)*sfdis(s,f,t)*VRSF(s,f,t,r))
                             +sum((p,f,w,t),tem(t)*fwdis(f,w,t)*VPFW(p,f,w,t))
                             +sum((p,w,c,t),tem(t)*wcdis(w,c,t)*VPWC(p,w,c,t));
totalco2..           co2 =e= pco2 + mco2 + tco2;                                        


* Objective functions
objcost..      z('cost') =e=  pc + mc + tc;

objwater..     z('water') =e= sum((s,f,r),swater(s,r)*VRP(s,f,r)) + sum((p,f),pwater(p)*VPS(p,f));


row1(s,r)..     scap(s,r)*VSB(s) =g= sum(f,VRP(s,f,r));

row2(f,r) ..    cr(f,r)*sum(s,VRP(s,f,r)) =e= sum(p,VPS(p,f));

row3(f)..       fcap(f)*VFB(f) =g= sum(p,VPS(p,f));

row5(p,w)..     sum((f,t),VPFW(p,f,w,t)) =e= sum((c,t),VPWC(p,w,c,t));

row6(c,p)..     sum((w,t),VPWC(p,w,c,t)) =g= dmcp(c,p);

row7(w)..       wcap(w)*VWB(w) =g= sum((p,f,t), VPFW(p,f,w,t));

row8(s,f,r)..   VRP(s,f,r) =e= sum(t, VRSF(s,f,t,r));

row9(f,p)..     VPS(p,f) =e= sum((w,t),VPFW(p,f,w,t));

Model supplychain / all /;
option optCR = 0;
*solve supplychain using MIP minimizing z;
*=== First unload to GDX file (occurs during execution phase)
*execute_unload "../results/result_%cp%.gdx";
*=== export results to excel file
*execute '=gdx2xls ../results/result_%cp%.gdx';





$STitle eps-Constraint Method
Set
   k1(k)  'the first element of k'
   km1(k) 'all but the first elements of k';

k1(k)$(ord(k) = 1) = yes;
km1(k)  = yes;
km1(k1) =  no;

Set kk(k) 'active objective function in constraint allobj';

Parameter
   rhs(k)     'right hand side of the constrained obj functions in eps-constraint'
   maxobj(k)  'maximum value from the payoff table'
   minobj(k)  'minimum value from the payoff table';

Variable
   a_objval   'auxiliary variable for the objective function'
   obj        'auxiliary variable during the construction of the payoff table';

Positive Variable
   sl(k)      'slack or surplus variables for the eps-constraints';

Equation
   con_obj(k) 'constrained objective functions'
   augm_obj   'augmented objective function to avoid weakly efficient solutions'
   allobj     'all the objective functions in one expression';

con_obj(km1).. z(km1) - dir(km1)*sl(km1) =e= rhs(km1);

* We optimize the first objective function and put the others as constraints
* the second term is for avoiding weakly efficient points
augm_obj.. sum(k1,dir(k1)*z(k1)) + 1e-3*sum(km1,sl(km1)/(maxobj(km1) - minobj(km1))) =e= a_objval;

allobj..   sum(kk, dir(kk)*z(kk)) =e= obj;

Model
   mod_payoff    / supplychain, allobj /
   mod_epsmethod / supplychain, con_obj, augm_obj /;

option limrow=0, limcol=0;
option solprint=off, solvelink=%solvelink.CallModule%;


option limRow = 0, limCol = 0, solPrint = off, solveLink = %solveLink.CallModule%;

Parameter payoff(k,k) 'payoff tables entries';

Alias (k,kp);

* Generate payoff table applying lexicographic optimization
loop(kp,
   kk(kp) = yes;
   repeat
      solve mod_payoff using MIP maximizing obj;
      payoff(kp,kk) = z.l(kk);
      z.fx(kk)      = z.l(kk); // freeze the value of the last objective optimized
      kk(k++1)      = kk(k);   // cycle through the objective functions
   until kk(kp);
   kk(kp) = no;
*  release the fixed values of the objective functions for the new iteration
   z.up(k) =  inf;
   z.lo(k) = -inf;
);

if(mod_payoff.modelStat <> %modelStat.optimal% and
   mod_payoff.modelStat <> %modelStat.feasibleSolution%,
   abort 'no feasible solution for mod_payoff');

display payoff;

minobj(k) = smin(kp,payoff(kp,k));
maxobj(k) = smax(kp,payoff(kp,k));

$set fname p.%gams.scrext%

File fx 'solution points from eps-method' / "%gams.scrdir%%fname%" /;

$if not set gridpoints $set gridpoints 30

Set
   g         'grid points' / g0*g%gridpoints% /
   grid(k,g) 'grid';

Parameter
   gridrhs(k,g) 'rhs of eps-constraint at grid point'
   maxg(k)      'maximum point in grid for objective'
   posg(k)      'grid position of objective'
   firstOffMax  'counter'
   lastZero     'counter'
   numk(k)      'ordinal value of k starting with 1'
   numg(g)      'ordinal value of g starting with 0';

lastZero = 1;
loop(km1,
   numk(km1) = lastZero;
   lastZero  = lastZero + 1;
);
numg(g) = ord(g) - 1;

grid(km1,g) = yes; // Here we could define different grid intervals for different objectives
maxg(km1)   = smax(grid(km1,g), numg(g));
gridrhs(grid(km1,g))$(%min% = dir(km1)) = maxobj(km1) - numg(g)/maxg(km1)*(maxobj(km1) - minobj(km1));
gridrhs(grid(km1,g))$(%max% = dir(km1)) = minobj(km1) + numg(g)/maxg(km1)*(maxobj(km1) - minobj(km1));
display gridrhs;

* Walk the grid points and take shortcuts if the model becomes infeasible
posg(km1) = 0;
scalar  iter  'total number of iterations';
iter = 0;
*option savepoint=2;
repeat
   iter = iter + 1;
   rhs(km1) = sum(grid(km1,g)$(numg(g) = posg(km1)), gridrhs(km1,g));
   solve mod_epsmethod maximizing a_objval using MIP;

   if(mod_epsmethod.modelStat <> %modelStat.optimal%,  // not optimal is in this case infeasible
      lastZero = 0;
      loop(km1$(posg(km1)  > 0 and lastZero = 0), lastZero = numk(km1));
      posg(km1)$(numk(km1) <= lastZero) = maxg(km1); // skip all solves for more demanding values of rhs(km1)
   else
      loop(k, put fx z.l(k):12:2); put /;
      Put_utility 'gdxout' / '../results/30/result_'iter:0:0'.gdx';
      Execute_unload;
      Put_utility 'exec' / 'gdx2xls  ../results/30/result_'iter:0:0'.gdx'
   );

*  Proceed forward in the grid
   firstOffMax = 0;
   loop(km1$(posg(km1) < maxg(km1) and firstOffMax = 0),
      posg(km1)   = posg(km1) + 1;
      firstOffMax = numk(km1);
   );
   posg(km1)$(numk(km1) < firstOffMax) = 0;
until sum(km1$(posg(km1) = maxg(km1)),1) = card(km1) and firstOffMax = 0;
putClose fx; // close the point file


* Get unique solutions from the point file using some Posix Tools (awk, (g)sort, uniq) that come with GAMS
$set awkscript awk.%gams.scrext%
file fa / "%gams.scrdir%%awkscript%" /; put fa 'BEGIN { printf("Table solutions(*,*)\n$ondelim\nsol';
loop(k, put ',' k.tl:0); putclose '\n"); }' / '{ print NR,$0 }' / 'END { print ";" }';
$if     %system.filesys% == UNIX execute 'cd "%gams.scrdir%" && sort %fname% | uniq | awk -f %awkscript% > g.%gams.scrext% && gams g.%gams.scrext% o=gx.%gams.scrext% lo=0 gdx=soleps';
$if NOT %system.filesys% == UNIX execute 'cd "%gams.scrdir%" && gsort %fname% | uniq | awk -f %awkscript% > g.%gams.scrext% && gams g.%gams.scrext% o=gx.%gams.scrext% lo=0 gdx=soleps';
execute 'mv -f "%gams.scrdir%soleps.gdx" .';

Set b Solutions /1*100/; Parameter solutions(b,k) Unique solutions;
execute_load 'soleps', solutions; display solutions;
$exit
