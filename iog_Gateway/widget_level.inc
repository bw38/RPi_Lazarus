//aus der unit sensor ausgelagerte Procedure zum Zeichnen der Level-Gauge
//Ergebnis in spng (MemoryStream) als png ablegen


procedure draw_widget_level(hSize: integer);

const ratio = 1.5; //width = hSize * ratio

var writer : TFPCustomImageWriter;
    w, h, ang, szval, szn, x, y, mxt, myt, h2, w2: integer;
    mcf, pA, pE, pT, pQ: TPoint;
    rsA1, rsA2, rsE, rsT, rsN, a: real;
    rval: TRect;
    tsz: TSize;
    s: ansistring;
    AFont: TFreeTypeFont;

    //Umrechnung in Prozent bezogen auf Gesamthöhe
    function XY(p: real): integer;
    begin
      result:= round(hSize * (p/100));
    end;


begin
  h:= hSize;
  w:= round(hSize * ratio);

  img := TFPMemoryImage.Create(w, h);
  can := TFPImageCanvas.Create(img);
  Writer := TFPWriterPNG.Create;

  ftfont.InitEngine;
  FontMgr.SearchPath:='/usr/share/fonts/truetype/dejavu/';
  AFont:= TFreeTypeFont.Create;
  can.Font:= AFont;

  mcf.x := XY(50);  //Mittelpunkt Ziffernblatt
  mcf.y := XY(50);

  can.Pen.FPColor := TColorToFPColor(clBlack);
  can.Pen.Width := 0;
  can.Brush.FPColor := TColorToFPColor(dsgn.outframe);
  can.Rectangle(0, 0, w-1 , h-1);
  can.Brush.FPColor := TColorToFPColor(dsgn.outEdge);
  can.Rectangle(XY(2), XY(2), w-XY(2), h-XY(2));
  can.Brush.FPColor := TColorToFPColor(dsgn.outBack);
  can.Rectangle(XY(4), XY(4), w-XY(4), h-XY(4));

  //Ziffernblatt
  can.Brush.FPColor := TColorToFPColor(dsgn.outFrame);
  can.Ellipse(mcf.x-XY(42), mcf.y-XY(42), mcf.x +XY(42), mcf.y+XY(42));
  can.Brush.FPColor := TColorToFPColor(dsgn.clockFace);
  can.Ellipse(mcf.x-XY(39), mcf.y-XY(39), mcf.x +XY(39), mcf.y+XY(39));
  //Skala
  rsA1:= XY(32); 	//Radien Skala
  rsA2:= XY(29);
  rsE:= XY(27);
  rsT:= XY(36);  //Scalenbeschriftung
  rsN:= XY(29);  //Needle

  can.Pen.FPColor := TColorToFPColor(dsgn.scaleLine);
	can.Pen.Width := XY(1.2);
  can.Font.Size := XY(3.2);
	can.Font.Name:= 'DejaVuSans';
  can.Font.FPColor := TColorToFPColor(dsgn.scaleText);

  ang:= 0;			//Text Skala
  while ang <= 100 do begin
    a:= (ang * 2.7 + 45) * pi / 180; //45° - 315° => 0 - 100%
    if ang mod 10 = 0 then begin
	  	pA.x:= mcf.x - round(rsA1*sin(a));   //lange Striche 10%
  		pA.y:= mcf.y + round(rsA1*cos(a));
      pE.x:= mcf.x - round(rsE*sin(a));
  		pE.y:= mcf.y + round(rsE*cos(a));
  		can.Line(pA, pE);
    end else if ang mod 5 = 0 then begin
      pA.x:= mcf.x - round(rsA2*sin(a));   //kurze Striche 5%
  		pA.y:= mcf.y + round(rsA2*cos(a));
    	pE.x:= mcf.x - round(rsE*sin(a));
  		pE.y:= mcf.y + round(rsE*cos(a));
  		can.Line(pA, pE);
    end;

		//Prozentwert Skala 0 / 1/2 / 1
    case ang of
    	0, 50, 100:
        begin
          if ang = 0 then s:= '0' else
          if ang = 50 then s:= '1/2' else
          s:= '1';
    			pT.x:= mcf.x - round(rsT*sin(a));
  				pT.y:= mcf.y + round(rsT*cos(a));
        	w2:= can.TextWidth(s) div 2;
        	h2:= can.TextHeight(s) div 2;
    			can.TextOut(pT.x-w2, pT.y+h2, s);
    		end;
    end; //case
    inc(ang);
  end;

  //min/max-Level
  a:= (dw_minlevel * 2.7 + 45) * pi / 180;
  pQ.x:= mcf.x - round(rsN*sin(a));
  pQ.y:= mcf.y + round(rsN*cos(a));
  can.MoveTo(pQ);
  can.Pen.FPColor := TColorToFPColor(dsgn.trend);
  can.Pen.Width:= XY(2);
  for ang:= dw_minlevel+1 to dw_maxlevel do begin
    a:= (ang * 2.7 + 45) * pi / 180;
    pQ.x:= mcf.x - round(rsN*sin(a));
    pQ.y:= mcf.y + round(rsN*cos(a));
    can.LineTo(pQ);
  end;

  //Feld f. mumerischen Wert im Ziffernblatt
  can.Pen.FPColor := TColorToFPColor(dsgn.outEdge);
  can.Pen.Width := XY(1);;
  can.Brush.FPColor := TColorToFPColor(dsgn.clockValField);
  szval:= XY(20);
  rval.Left := mcf.x - szval div 2;
  rval.Right := rval.Left + szval;

  rval.Top:= mcf.y + XY(20);
  rval.Bottom:= rval.Top + round(szval*0.6);
  can.Rectangle(rval);

  //Numerischer Wert im Ziffernblatt
  s:= IntToStr(dw_level) + ' %';
  can.Font.Size := XY(3.5);
  can.Font.FPColor := TColorToFPColor(dsgn.clockValTxt);
  can.Brush.FPColor := TColorToFPColor(dsgn.clockValField);
  tsz:= can.TextExtent(s);
  x:= (rval.Right-rval.Left) div 2 - tsz.cx div 2;
  y:= (rval.Bottom-rval.Top) div 2 + tsz.cy div 2;
  can.Textout(rval.Left+x, rval.Top+y, s);

  //Nadel
  a:= dw_level*0.015*pi + 0.25*pi;
  can.Pen.FPColor := TColorToFPColor(dsgn.needle);
  can.Pen.Width := XY(1.8);
  pE.x:= mcf.x - round(rsN*sin(a));   //lange Striche 10%
  pE.y:= mcf.y + round(rsN*cos(a));
  can.Line(mcf, pE);
  can.Brush.FPColor := TColorToFPColor(dsgn.needle);
  szn:= XY(8) div 2;
  can.Ellipse(mcf.x - szn, mcf.y - szn, mcf.x + szn, mcf.y + szn  );

  //Texte
  mxt:= XY(115); //Centerline der Textausgaben
  can.Brush.FPColor := TColorToFPColor(dsgn.outBack);
  can.font.FPColor := TColorToFPColor(dsgn.txt);

  //Name
  can.Font.Size := XY(4);
  tsz:= can.TextExtent(dw_idtxt);
  y:= XY(15);
  can.TextOut(mxt-tsz.cx div 2, y, dw_idtxt);
  //Timestamp
  s:= FormatDateTime('dd.mm.YY - hh:nn:ss', dw_sensortime);
  can.Font.Size := XY(3.5);;
  tsz:= can.TextExtent(s);
  y:= XY(30);
  can.TextOut(mxt-tsz.cx div 2, y, s);
  //Temperatur
  s:= 'Temp: ' + FloatToStrF(dw_temp, ffFixed, 7,1) + #176+'C';  //°C
  tsz:= can.TextExtent(s);
  y:= XY(45);
  can.TextOut(mxt-tsz.cx div 2, y, s);
  //Min-Max-Temp
  s:= '[ ' +
  	FloatToStrF(dw_mintemp, ffFixed, 7,1) + ' / ' +
  	FloatToStrF(dw_maxtemp, ffFixed, 7,1) + #176+ 'C ]';
  tsz:= can.TextExtent(s);
  y:= XY(50);
  can.TextOut(mxt-tsz.cx div 2, y, s);

  can.Font.Size := XY(3);
  myt:= 65;	//Y-Pos der Textzeilen
  //Version
  s:= 'Version: ' + dw_strVersion;
  tsz:= can.TextExtent(s);
  y:= XY(myt+0);
  can.TextOut(mxt-tsz.cx div 2, y, s);
  //Batteriespannung
  s:= 'Batterie: ' + FloatToStrF(dw_ubatt, ffFixed, 7,2) + 'V';  //°C
  tsz:= can.TextExtent(s);
  y:= XY(myt+5);
  can.TextOut(mxt-tsz.cx div 2, y, s);
  //UpTime
  s:= 'UpTime: ' + IntToStr(dw_UpTime) + 'Sek';
  tsz:= can.TextExtent(s);
  y:= XY(myt+10);
  can.TextOut(mxt-tsz.cx div 2, y, s);
  //Interval
  s:= 'Interval: ' + IntToStr(dw_interval) + 'min';
  tsz:= can.TextExtent(s);
  y:= XY(myt+15);
  can.TextOut(mxt-tsz.cx div 2, y, s);
  //Sendeversuche
  s:= 'TxShot: ' + IntToStr(dw_cntTxShot);
  tsz:= can.TextExtent(s);
  y:= XY(myt+20);
  can.TextOut(mxt-tsz.cx div 2, y, s);
  //Sendefehler am Tag
  s:= 'TxFehler: ' + IntToStr(dw_errTxShot);
  tsz:= can.TextExtent(s);
  y:= XY(myt+25);
  can.TextOut(mxt-tsz.cx div 2, y, s);

 // writeln('Dauer1: ', IntToStr( GetTickCount64() - time));

  spng.Clear;
  img.SaveToStream(spng, writer);
  //writeln('Dauer2: ', IntToStr( GetTickCount64() - time));
  can.Free;
  img.Free;
  writer.Free;

end;
