$onSymXRef
$title  'aluminum model'


$if %system.filesys% == UNIX $abort.noerror 'This model cannot run on a non-Windows platform';
$call msappavail -Excel
$if errorlevel 1 $abort.noerror 'Microsoft Excel is not available!';


Set

   s 'supplier'              / Al_supplier_1,  Al_supplier_2, Al_supplier_3 /
   f 'plant'                 / plant_1, plant_2, plant_3 /
   w 'warehouse'             / warehouse_1, warehouse_2 /
   c 'customer'              / customer_1, customer_2, customer_3/
   t 'transport mode'        / road,  rail, barge /
   p 'product'               / product_A, product_B /
   r 'raw material type'     / Al /
   ;

$if not set cp $set cp 100
Scalar cp        'carbon price (yuan)'                                             / %cp% /
       lifetime  'lifetime'                                                        / 25 /
       ir        'interest rate'                                                   / 0.06 /
    ;

Parameter

   ssel(s)   'supplier selection cost'

   scap(s,r) 'capacity (ton) for raw material r of supplier s'

   sem(s,r) 'CO2 emissions(ton) for puducting one ton raw material r in supplier s'

   sprc(s,r)   'price (yuan) of one ton raw material r for supplier s'

   swater(s,r) 'water consumed (ton) for making one ton raw material r for supplier s'

   cr(f,r)  'conversion rate from feedstock to product'

   fcap(f) 'total capacity (ton) in plant f'

   ftc(f)  'total fixed costs (yuan) of establishing plant f'

   fce(f) 'annual fixed costs (yuan) of establishing plant f'

   pcep(p) 'variable costs(yuan) of producing one ton of product p'

   pwater(p) 'water(ton) of producing one ton of producr p'

   wcap(w) 'total capacity (ton) in warehouse w'

   wtc(w)  'total fixed costs (yuan) of establishing warehouse w'

   wce(w) 'annual fixed costs (yuan) of establishing warehouse w'

   pup(p) 'price (yuan) per ton of product p'

   pemp(p) 'CO2 emissions(ton) for producing one ton of product p'

   sfcost(s,f,t) 'shipping cost (yuan per km per ton) from supplier s to plant f in transport mode t'

   sfdis(s,f,t)    'distance (km) from supplier s to plant f in transport mode t'

   fwcost(f,w,t) 'shipping cost (yuan per km per ton) from  plant f  to warehouse w in transport mode t'

   fwdis(f,w,t) 'distance (km)  from  plant f  to warehouse w in transport mode t'

   wccost(w,c,t) 'shipping cost (yuan per km per ton) from warehouse w to customer c in transport mode t'

   wcdis(w,c,t) 'distance (km) from warehouse w to customer c in transport mode t'

   tem(t) 'CO2 emissions(ton/(ton,km) in transport mode t'

   dmcp(c,p) 'demand (tons) of product p of customer c'
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

z 'overall costs'

wt 'water consumed'

pc 'purchasing cost of raw materials'

mc 'manufacturing and storage cost'

tc 'transportation cost'

pco2 'amount of co2 in purchasing process'

mco2 'amount of co2 in manufacturing process'

tco2 'amount of co2 in transportation process'
;


equation

cost                'objective function include suppliercost and plantcost'

water               'second objective function'

purchasingcost      'purchasing cost of raw materials'

manufacturingcost   'manufacturing and storage cost'

transportationcost  'transportation cost'

purchasingco2       'amount of co2 in purchasing process'

manufacturingco2    'amount of co2 in manufacturing process'

transportationco2   'amount of co2 in transportation process'

row1(s,r)           'supplier capacity constraints'

row2(f,r)          'amounts of raw material r saticify production'

row3(f)            'plant capacity constraints'

row5(p,w)          'inflow of products to warehouse equal to outflow '

row6(c,p)          'amounts of products transported from warehouse to customers equal to their demands'

row7(w)            'warehouse capacity constraints'

row8(s,f,r)        'feedstock transport to plant'

row9(f,p)          'amounts of products of plant equals to outflow'


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


purchasingco2..    pco2 =e= sum((s,f,r), sem(s,r)*VRP(s,f,r));

manufacturingco2.. mco2 =e= sum((p,f), pemp(p)*VPS(p,f));

transportationco2.. tco2 =e=  sum((s,f,t,r),tem(t)*sfdis(s,f,t)*VRSF(s,f,t,r))
                             +sum((p,f,w,t),tem(t)*fwdis(f,w,t)*VPFW(p,f,w,t))
                             +sum((p,w,c,t),tem(t)*wcdis(w,c,t)*VPWC(p,w,c,t));


cost ..         z =e=  pc + mc + tc;

water..         wt =e= sum((s,f,r),swater(s,r)*VRP(s,f,r)) + sum((p,f),pwater(p)*VPS(p,f));

row1(s,r)..     scap(s,r)*VSB(s) =g= sum(f,VRP(s,f,r));

row2(f,r) ..    cr(f,r)*sum(s,VRP(s,f,r)) =e= sum(p,VPS(p,f));

row3(f)..       fcap(f)*VFB(f) =g= sum(p,VPS(p,f));

row5(p,w)..     sum((f,t),VPFW(p,f,w,t)) =e= sum((c,t),VPWC(p,w,c,t));

row6(c,p)..     sum((w,t),VPWC(p,w,c,t)) =g= dmcp(c,p);

row7(w)..       wcap(w)*VWB(w) =g= sum((p,f,t), VPFW(p,f,w,t));

row8(s,f,r)..   VRP(s,f,r) =e= sum(t, VRSF(s,f,t,r));

row9(f,p)..     VPS(p,f) =e= sum((w,t),VPFW(p,f,w,t));



Model supplychain / ALL /;
option optCR = 0;
solve supplychain using MIP minimizing z;

*=== First unload to GDX file (occurs during execution phase)
execute_unload "../results/result_%cp%.gdx";
*=== export results to excel file
execute '=gdx2xls ../results/result_%cp%.gdx';

