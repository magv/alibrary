#include color.h

#define MaxGammaTracesPerTerm "6"
#define MaxGamma5PerTerm "4"

* These are the internal names.
CF EX, UNIQTAG;
S FAIL;
auto V tmpv;
auto S x;

Table sparse zerofill UNIQVALS(1);

* These are the incoming and outgoing names.
cfunction delta(s), epsilon4(a), deltaf(s), deltaft(s), dot(s), momentum, polarization, den, gammachain, gammatrace, spinor, slash, gamma, gamma5, colorT, colorf, sp, B;
cfunction chargeQ, chargeV, chargeA, chargeQt, chargeVt, chargeAt;
cfunction flvsum, flvsumt;
cfunction inv;
symbol I, d, ep, cut, irr;
symbol Ca, Cf, Tf, Na, Nc, Xi, Xi2, Xi3, Xi4;
symbol Nf, Nft, Q0, V0, A0, Q02, V02, A02, V0A0, SumQf, SumQf2, SumVf, SumVf2, SumAf, SumAf2, SumVfAf;
symbol BID;
auto symbol g;
auto index lor = d, adj = Na, fun = Nc, flv = FAIL, spn = FAIL, X = FAIL;
auto vector p, q, l, r;
auto symbol DEN;
auto symbol m, s;
cfunction A, P;

unitTrace 4;

#procedure input
#call begin(input)
    id delta(lor1?, lor2?) = d_(lor1, lor2);
    id I = i_;
#call end(input)
#endprocedure

#procedure output(file)
#call begin(output)
    id d_(lor1?, lor2?) = delta(lor1, lor2);
    id i_ = I;
    id p1?.p2? = dot(p1, p2);
    bracket EX,B;
    #call sort(output)

    format Mathematica;
    format nospaces;
    format 78;
    #write <`file'> "(\n      %E\n)" EXPR;
    #close <`file'>
#call end(output)
#endprocedure

#procedure flavorsumwithcharge()
#call begin(flavorsumwithcharge)
* The flavor deltas may have these structures:
* - d[ab] d[bc] d[ca]             -- a closed loop (=Nf)
* - d[ab] d[bc] d[ca] charge[b]^n -- a closed loop with a charge (=SumQf, SumQf2)
* - d[ab] d[b0] d[0a]             -- a fixed index (=1)
* - d[ab] d[b0] d[0a] charge[b]^n -- a fixed index with a charge (=Q0, Q0^2)
    repeat id deltaf(flv1?, flv?)*deltaf(flv?, flv2?) = deltaf(flv1, flv2)*replace_(flv, flv1);
    repeat id deltaft(flv1?, flv?)*deltaft(flv?, flv2?) = deltaft(flv1, flv2)*replace_(flv, flv1);
* Now we only have:
* - d[aa]
* - d[aa] charge[a]^n
    id deltaf(flv?, flv?) = flvsum(flv, 1);
    repeat id flvsum(flv?, x?) * chargeQ(flv?) = flvsum(flv, x*chargeQ);
    repeat id flvsum(flv?, x?) * chargeV(flv?) = flvsum(flv, x*chargeV);
    repeat id flvsum(flv?, x?) * chargeA(flv?) = flvsum(flv, x*chargeA);
    id flvsum(flv?, x?) = flvsum(x);
    if (match(deltaf(x1?, x2?)) || match(chargeQ(?x)) || match(chargeA(?x)) || match(chargeV(?x)));
      exit "ERROR: flavorsumwithcharge: leftover light flavors; unexpected flavor structure?";
    endif;
* Same but for heavy flavors.
    id deltaft(flv?, flv?) = flvsumt(flv, 1);
    repeat id flvsumt(flv?, x?) * chargeQt(flv?) = flvsumt(flv, x*chargeQt);
    repeat id flvsumt(flv?, x?) * chargeVt(flv?) = flvsumt(flv, x*chargeVt);
    repeat id flvsumt(flv?, x?) * chargeAt(flv?) = flvsumt(flv, x*chargeAt);
    id flvsumt(flv?, x?) = flvsumt(x);
    if (match(deltaft(x1?, x2?)) || match(chargeQt(?x)) || match(chargeAt(?x)) || match(chargeVt(?x)));
      exit "ERROR: flavorsumwithcharge: leftover heavy flavors; unexpected flavor structure?";
    endif;
#call end(flavorsumwithcharge)
#endprocedure

#procedure flavorsum(deltaf, Nf)
#call begin(flavorsum(`deltaf' `Nf'))
    repeat id `deltaf'(flv1?, flv?!{,0})*`deltaf'(flv?, flv2?) = `deltaf'(flv1, flv2)*replace_(flv, flv1);
    id `deltaf'(flv?, flv?) = `Nf';
    if (match(`deltaf'(x1?, x2?)));
      exit "ERROR: flavorsum: leftover flavors; unexpected flavor structure?";
    endif;
#call end(flavorsum(`deltaf' `Nf'))
#endprocedure

#procedure chaingammachain
#call begin(chaingammachain)
    repeat id gammachain(?x1, spn1?, spn2?)*gammachain(?x2, spn2?, spn3?) = gammachain(?x1, ?x2, spn1, spn3);
    id gammachain(?x, spn?, spn?) = gammatrace(?x);
#call end(chaingammachain)
#endprocedure

#procedure chaincolorT
#call begin(chaincolorT)
    repeat id colorT(?adj1, fun1?, fun2?)*colorT(?adj2, fun2?, fun3?) = colorT(?adj1, ?adj2, fun1, fun3);
#call end(chaincolorT)
#endprocedure

#procedure polarizationsum
#call begin(polarizationsum)
    repeat id polarization(p?, lor1?, -I)*polarization(p?, lor2?, I) = -d_(lor1, lor2);
#call end(polarizationsum)
#endprocedure

#procedure spinsum
#call begin(spinsum)
    if (match(gammachain(?x)));
      exit "ERROR: spinsum: unchained gammachain found; did chaingammachain fail?";
    endif;
    if (match(spinor(?x)));
      exit "ERROR: spinsum: spinor outside of gammatrace found; did chaingammachain fail?";
    endif;
    if (match(gamma(?x)));
      exit "ERROR: spinsum: gamma outside of gammatrace found; did chaingammachain fail?";
    endif;
    repeat id gammatrace(spinor(p?, -I), ?x1, spinor(p?, I)) = gammatrace(?x1, slash(p));
    repeat id gammatrace(?x1, spinor(p?, I), spinor(p?, -I), ?x2) = gammatrace(?x1, slash(p), ?x2);
    if (match(gammatrace(?x1, spinor(?x2), ?x3)));
      exit "ERROR: spinsum: spinors remain in gammatraces after the spin sum.";
    endif;
#call end(spinsum)
#endprocedure

#procedure diractrace
#call begin(diractrace)
    #call uniqbegin(gammatrace d_ epsilon4)
    #if ( `EXTRASYMBOLS_' > 0 )
        exit "ERROR: diractrace: already have `EXTRASYMBOLS_' extra symbols at the start";
    #endif
* Rename unique momenta into symbols to prevent their dot product
* expansion and subsequent expression swell.
    argument gammatrace;
        argToExtraSymbol tonumber slash,1;
    endargument;
    #call sort(diractrace-rename)

    #write "*** uniq momenta combinations: `EXTRASYMBOLS_'"
    #write "%X"
* Larin scheme for gamma5 involved in an axial current.
    #do i = 1, `MaxGamma5PerTerm'
        id once gammatrace(?x1, gamma5(lormu?), ?x2) =
            i_/6 * epsilon4(lormu, lorax`i'a, lorax`i'b, lorax`i'c) *
            gammatrace(?x1, gamma(lorax`i'a), gamma(lorax`i'b), gamma(lorax`i'c), ?x2);
    #enddo
    argument gammatrace;
        #do i = 1,`EXTRASYMBOLS_'
            id slash(`i') = tmpv`i';
        #enddo
        id gamma(lor?) = lor;
    endargument;
    #do i = 1, `MaxGammaTracesPerTerm'
        id once gammatrace(?x) = g_(`i', ?x);
    #enddo
    #call contractepsilon4()
    #do i = 1, `MaxGammaTracesPerTerm'
        traceN,`i';
    #enddo
    if (match(epsilon4(lor1?, lor2?, lor3?, lor4?)));
      exit "ERROR: diractrace: leftover epsilon4() at the end; is MaxGammaTracesPerTerm too small?";
    endif;
    if (match(gammatrace(?x)));
      exit "ERROR: diractrace: leftover gammatrace() at the end; is MaxGammaTracesPerTerm too small?";
    endif;
    #call sort(diractrace-traceN)
    id p1?.p2? = dot(p1, p2);
    id p1?(lor1?) = momentum(p1, lor1);
    id momentum(p1?, p2?) = dot(p1, p2);
    argument momentum, dot;
        #do i=1,`EXTRASYMBOLS_'
            id tmpv`i' = extrasymbol_(`i');
        #enddo
    endargument;
* This sort is only needed because we want to delete extra symbols
* in this procedure...
    #call sort(diractrace-rename-back)

    delete extrasymbols;
    #call uniqend()
#call end(diractrace)
#endprocedure

#procedure contractepsilon4
#call begin(contractepsilon4)
* If less than 4 external momenta are present in the problem, then
* a single epsilon4 must contract with a symmetric combination
* of momenta, and thus must become zero.
*    id epsilon4(lor1?, lor2?, lor3?, lor4?) = xN * epsilon4(lor1, lor2, lor3, lor4);
*    id xN^xNPOW?odd_ = 0;
*    id xN = 1;
    repeat id epsilon4(lor1?, lor2?, lor3?, lorx?)*epsilon4(lorA?, lorB?, lorC?, lorx?) = (
       - d_(lor1,lorA)*d_(lor2,lorB)*d_(lor3,lorC)*3
       + d_(lor1,lorA)*d_(lor2,lorB)*d_(lor3,lorC)*d
       + d_(lor1,lorA)*d_(lor2,lorC)*d_(lor3,lorB)*3
       - d_(lor1,lorA)*d_(lor2,lorC)*d_(lor3,lorB)*d
       + d_(lor1,lorB)*d_(lor2,lorA)*d_(lor3,lorC)*3
       - d_(lor1,lorB)*d_(lor2,lorA)*d_(lor3,lorC)*d
       - d_(lor1,lorB)*d_(lor2,lorC)*d_(lor3,lorA)*3
       + d_(lor1,lorB)*d_(lor2,lorC)*d_(lor3,lorA)*d
       - d_(lor1,lorC)*d_(lor2,lorA)*d_(lor3,lorB)*3
       + d_(lor1,lorC)*d_(lor2,lorA)*d_(lor3,lorB)*d
       + d_(lor1,lorC)*d_(lor2,lorB)*d_(lor3,lorA)*3
       - d_(lor1,lorC)*d_(lor2,lorB)*d_(lor3,lorA)*d
    );
    repeat id epsilon4(lor1?, lor2?, lor3?, lor4?)*epsilon4(lorA?, lorB?, lorC?, lorD?) = (
       + d_(lor1,lorA)*d_(lor2,lorB)*d_(lor3,lorC)*d_(lor4,lorD)
       - d_(lor1,lorA)*d_(lor2,lorB)*d_(lor3,lorD)*d_(lor4,lorC)
       - d_(lor1,lorA)*d_(lor2,lorC)*d_(lor3,lorB)*d_(lor4,lorD)
       + d_(lor1,lorA)*d_(lor2,lorC)*d_(lor3,lorD)*d_(lor4,lorB)
       + d_(lor1,lorA)*d_(lor2,lorD)*d_(lor3,lorB)*d_(lor4,lorC)
       - d_(lor1,lorA)*d_(lor2,lorD)*d_(lor3,lorC)*d_(lor4,lorB)
       - d_(lor1,lorB)*d_(lor2,lorA)*d_(lor3,lorC)*d_(lor4,lorD)
       + d_(lor1,lorB)*d_(lor2,lorA)*d_(lor3,lorD)*d_(lor4,lorC)
       + d_(lor1,lorB)*d_(lor2,lorC)*d_(lor3,lorA)*d_(lor4,lorD)
       - d_(lor1,lorB)*d_(lor2,lorC)*d_(lor3,lorD)*d_(lor4,lorA)
       - d_(lor1,lorB)*d_(lor2,lorD)*d_(lor3,lorA)*d_(lor4,lorC)
       + d_(lor1,lorB)*d_(lor2,lorD)*d_(lor3,lorC)*d_(lor4,lorA)
       + d_(lor1,lorC)*d_(lor2,lorA)*d_(lor3,lorB)*d_(lor4,lorD)
       - d_(lor1,lorC)*d_(lor2,lorA)*d_(lor3,lorD)*d_(lor4,lorB)
       - d_(lor1,lorC)*d_(lor2,lorB)*d_(lor3,lorA)*d_(lor4,lorD)
       + d_(lor1,lorC)*d_(lor2,lorB)*d_(lor3,lorD)*d_(lor4,lorA)
       + d_(lor1,lorC)*d_(lor2,lorD)*d_(lor3,lorA)*d_(lor4,lorB)
       - d_(lor1,lorC)*d_(lor2,lorD)*d_(lor3,lorB)*d_(lor4,lorA)
       - d_(lor1,lorD)*d_(lor2,lorA)*d_(lor3,lorB)*d_(lor4,lorC)
       + d_(lor1,lorD)*d_(lor2,lorA)*d_(lor3,lorC)*d_(lor4,lorB)
       + d_(lor1,lorD)*d_(lor2,lorB)*d_(lor3,lorA)*d_(lor4,lorC)
       - d_(lor1,lorD)*d_(lor2,lorB)*d_(lor3,lorC)*d_(lor4,lorA)
       - d_(lor1,lorD)*d_(lor2,lorC)*d_(lor3,lorA)*d_(lor4,lorB)
       + d_(lor1,lorD)*d_(lor2,lorC)*d_(lor3,lorB)*d_(lor4,lorA)
    );
#call end(contractepsilon4)
#endprocedure

#procedure contractmomenta
#call begin(contractmomenta)
    repeat;
        id momentum(p1?, lor?)*momentum(p2?, lor?) = dot(p1, p2);
        id momentum(p1?, lor?)^2 = dot(p1, p1);
        id momentum(p1?, p2?) = dot(p1, p2);
    endrepeat;
#call end(contractmomenta)
#endprocedure

#procedure expanddots
#call begin(expanddots)
    id dot(p1?, p2?) = p1.p2;
    id p1?.p2? = dot(p1, p2);
#call end(expanddots)
#endprocedure

#procedure colorsum
#call begin(colorsum)
    id colorT(?adj, lor1?, lor2?) = T(lor1, lor2, ?adj);
    id colorf(?adj) = f(?adj);
* We shall only perform color summation on unique colorf and
* colorT combinations, hiding the rest of the expression away
* for performance.
    #call uniqbegin(f T)
    #call docolor
    #call uniqend
* Change from color.h notation to ours.
    id cA = Ca;
    id cR = Cf;
    id I2R = Tf;
    id NA = Na;
    id NR = Nc;
    id d33(cOlpR1?, cOlpR2?) = d33;
    id d44(cOlpR1?, cOlpR2?) = d44;
    id T(lor1?, lor2?, ?adj) = colorT(?adj, lor1, lor2);
    id f(adj1?, adj2?, adj3?) = colorf(adj1, adj2, adj3);
#call end(colorsum)
#endprocedure

* Convert an expression with dot(...) and den(...) into the IBP
* B(...) notation. Most of the time is actually taken by the
* dot(...) expansion, so extra care is devoted to it.
*
* Example:
*   #procedure toBID
*     if (match(EX(x?{,1,2,3})));
*       multiply BID^1;
*     elif (match(EX(x?{,4,5,6})));
*       multiply BID^2;
*     endif;
*   #endprocedure
*   #procedure toDEN
*     if (match(only, BID^1));
*       id den(l1) = DEN1;
*       id l1.l1 = DEN1^(-1);
*       ...
*     elif (match(only, BID^2));
*       id den(l1+l2) = DEN1;
*       id l1.l2 = DEN1^(-1)-DEN2^(-1)-DEN3^(-1);
*       ...
*     endif;
*   #endprocedure
*   #call toB(5, toBID, toDEN)
#procedure toB(ndens, toBID, toDEN)
#call begin(toB)
    if (match(momentum(p?, lor?)));
      exit "ERROR: toB: still have momentum(); did diractrace fail?";
    endif;
    #call `toBID'
* First determine the set of unique BID^n*dot(...) expressions,
* then convert them to DEN combinations, and finally substitute
* the results back, sorting after each substitution round.
    #if ( `EXTRASYMBOLS_' > 0 )
        exit "ERROR: toB: already have `EXTRASYMBOLS_' extra symbols at the start";
    #endif
    repeat id once BID^x? * dot(p1?, p2?) =  BID^x * UNIQTAG(BID^x * dot(p1, p2));
    argToExtraSymbol tonumber UNIQTAG,1;

    #call `toDEN'
    if (match(den(?x)));
      exit "ERROR: toB: still have den() after IBP basis substitution; bad basis?";
    endif;

    #call sort(toB-uniq-dots)
    #write "*** uniq dots*basis combinations: `EXTRASYMBOLS_'"
    #write "%X"

    pushHide;
    L DOTS =
    #if `EXTRASYMBOLS_' > 0
        <UNIQTAG(1)*extrasymbol_(1)> + ... + <UNIQTAG(`EXTRASYMBOLS_')*extrasymbol_(`EXTRASYMBOLS_')>
*        sum_(xidx,1,`EXTRASYMBOLS_',UNIQTAG(xidx)*extrasymbol_(xidx)>
    #else
        0
    #endif
    ;
    id dot(p1?, p2?) = p1.p2;
    #call `toDEN'
* Scalar products that did not become DENs must be external
* invariants. We shall mark them with sp(...), as opposed to
* dot(...), which is what is expected everywhere else.
    id p1?.p1? = sp(p1);
    id p1?.p2? = sp(p1, p2);
    id BID^x? = 1;
    bracket UNIQTAG;
    #call sort(toB-convert-uniq-dots)

    table DOTEXP(1:`EXTRASYMBOLS_');
    fillExpression DOTEXP = DOTS(UNIQTAG);
    drop DOTS;
    popHide;

    #define tobloop "1"
    #do tobloopdone = 1, 1
        id once UNIQTAG(x?) = DOTEXP(x);
        if (match(UNIQTAG(?x))) redefine tobloopdone "0";
        #call sort(toB-expand-`tobloop')
        #redefine tobloop "{`tobloop' + 1}"
    #enddo

    #if ( `ndens' > 0 )
    id BID^x0? * <DEN1^x1?> * ... * <DEN`ndens'^x`ndens'?> = B(x0, <x1>, ..., <x`ndens'>);
    #else
    id BID^x? = 1;
    #endif
#call end(toB)
#endprocedure

* Call uniqbegin(a b c) to extract factors composed of a b c from
* each term, find the unique ones, and leave only one expression
* active: a sum of these unique combinators multiplied by UNIQTAG
* tags. After this you can transform these factors as you wish.
*
* A call to uniqend() will put the result of the transformation
* back into the original expression.
#procedure uniqbegin(bracketargs)
#call begin(uniqbegin)
    #if ( `EXTRASYMBOLS_' > 0 )
        exit "ERROR: uniqbegin: already have `EXTRASYMBOLS_' extra symbols at the start";
    #endif
* We want to put bracketargs into the UNIQTAG; putInside will
* do that, but it will pull in all the numerical coefficients
* too (why?). We'll use factArg to pull out those coefficient
* into a separate argument. Note that factArg will sometimes
* fail to pull out a coefficient 1: specifically for UNIQTAG(1)
* and UNIQTAG(t()), but not for UNIQTAG(x). No idea why. None
* of this is documented, of course.
    putInside UNIQTAG `bracketargs';
    factArg (-1) UNIQTAG;
* We need an extra sort here because FORM doesn't allow factArg
* and argToExtraSymbol to appear in the same block.
    #call sort(uniqbegin-pre)

    argToExtraSymbol tonumber UNIQTAG,1;
    #call sort(uniqbegin-rename)

    clearTable UNIQVALS;
    pushHide;
    #write "*** uniq expressions: `EXTRASYMBOLS_'"
    #write "%X"
    L UNIQEXPR = <extrasymbol_(1)*UNIQTAG(1)>+...+<extrasymbol_(`EXTRASYMBOLS_')*UNIQTAG(`EXTRASYMBOLS_')>;
    #call sort(uniqbegin-distribute)

    delete extrasymbols;
#call end(uniqbegin)
#endprocedure

* See uniqbegin().
#procedure uniqend()
#call begin(uniqend)
    bracket UNIQTAG;
    #call sort(uniqend-bracket)

    fillExpression UNIQVALS = UNIQEXPR(UNIQTAG);
    drop UNIQEXPR;
    popHide;
    id UNIQTAG(x?, xx?) = xx*UNIQVALS(x);
    id UNIQTAG(x?) = UNIQVALS(x);
#call end(uniqend)
#endprocedure

#procedure dropterms(pattern)
#call begin(dropterms(`pattern'))
    id `pattern' = 0;
#call end(dropterms(`pattern'))
#endprocedure

* Log the start of a procedure.
#procedure begin(procname)
#write ">>> `procname' (`TIME_'s)"
#reset timer
#endprocedure

* Log the end of a procedure.
#procedure end(procname)
#write "<<< `procname' (`TIME_'s, +{`TIMER_' / 1000}.{`TIMER_' / 10 % 10}{`TIMER_' % 10}s)"
#endprocedure

* Sort, logging the time.
#procedure sort(tag)
#call begin(sort(`tag'))
    #ifdef `DEBUG'
    print +s;
    #endif
    .sort:`tag';
#call end(sort(`tag'))
#endprocedure
