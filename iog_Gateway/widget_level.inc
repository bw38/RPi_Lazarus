//aus der unit sensor ausgelagerte Procedure zum Zeichnen der Level-Gauge
//Ergebnis in spng (MemoryStream) als png ablegen

procedure draw_widget_level(hSize: integer);

const ratio = 1.5; //width = hSize * ratio

var can : TFPCustomCanvas;
    img : TFPCustomImage;
    writer : TFPCustomImageWriter;
    w, h, ang, szval, sz, x, y, mxt, h2, w2: integer;
    mcf, pA, pE, pT: TPoint;
    rsA1, rsA2, rsE, rsT, rsN, a: real;
    rval: TRect;
    tsz: TSize;
    s: ansistring;
    AFont: TFreeTypeFont;
    time: cardinal;

    //Umrechnung in Prozent bezogen auf Gesamthöhe
    function XY(p: real): integer;
    begin
      result:= round(hSize * (p/100));
    end;


begin
  time:= GetTickCount();
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
  a:= 0.25*pi; 	//Anfangswinkel
  while ang <= 100 do begin
    if ang mod 2 = 0 then begin
	  	pA.x:= mcf.x - round(rsA1*sin(a));   //lange Striche 10%
  		pA.y:= mcf.y + round(rsA1*cos(a));
    end else begin
      pA.x:= mcf.x - round(rsA2*sin(a));   //kurze Striche 5%
  		pA.y:= mcf.y + round(rsA2*cos(a));
    end;
    pE.x:= mcf.x - round(rsE*sin(a));
  	pE.y:= mcf.y + round(rsE*cos(a));
  	can.Line(pA, pE);

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

  	a:= a + 0.075*pi;
    ang:= ang + 5;
  end;


    //Feld f. mumerischen Wert im Ziffernblatt
  can.Brush.FPColor := TColorToFPColor(dsgn.clockValField);
  szval:= XY(20);
  rval.Left := mcf.x - szval div 2;
  rval.Right := rval.Left + szval;

  rval.Top:= mcf.y + XY(20);
  rval.Bottom:= rval.Top + round(szval*0.6);
  can.Rectangle(rval);

  //Numerischer Wert im Ziffernblatt
  s:= IntToStr(level) + ' %';
  can.Font.Size := XY(3.5);
  can.Font.FPColor := TColorToFPColor(dsgn.clockValTxt);
  can.Brush.FPColor := TColorToFPColor(dsgn.clockValField);
  tsz:= can.TextExtent(s);
  x:= (rval.Right-rval.Left) div 2 - tsz.cx div 2;
  y:= (rval.Bottom-rval.Top) div 2 + tsz.cy div 2;
  can.Textout(rval.Left+x, rval.Top+y, s);

  //Min- / Max-Level
  can.Pen.FPColor := TColorToFPColor(dsgn.trend);
  can.Pen.Width:= XY(2.8);
  x:= round(rsN);
  can.Arc(
  	mcf.x - x, mcf.y - x, mcf.x + x, mcf.y + x, //Quadrat des Kreises
  	round(225 * 16 + minlevel * 2.7 * -16),			//auf 0% zurückdrehen (225°) - Minwert
    round((maxlevel-minlevel) * 2.7 * -16));		//Winkel Maxwert bezogen auf 270° = 100%

  //Nadel
  a:= level*0.015*pi + 0.25*pi;
  can.Pen.FPColor := TColorToFPColor(dsgn.needle);
  can.Pen.Width := XY(1.8);
  pE.x:= mcf.x - round(rsN*sin(a));   //lange Striche 10%
  pE.y:= mcf.y + round(rsN*cos(a));
  can.Line(mcf, pE);
  can.Brush.FPColor := TColorToFPColor(dsgn.needle);
  sz:= XY(8) div 2;
  can.Ellipse(mcf.x - sz, mcf.y - sz, mcf.x + sz, mcf.y + sz  );

  //Texte
  mxt:= XY(115); //Centerline der Textausgaben
  can.Brush.FPColor := TColorToFPColor(dsgn.outBack);
  can.font.FPColor := TColorToFPColor(dsgn.txt);

  //Name
  can.Font.Size := XY(5);
  tsz:= can.TextExtent(idtxt);
  y:= XY(12);
  can.TextOut(mxt-tsz.cx div 2, y, idtxt);
  //Timestamp
  s:= FormatDateTime('dd.mm.YY - hh:nn:ss', time);
  can.Font.Size := XY(4);;
  tsz:= can.TextExtent(s);
  y:= XY(24);
  can.TextOut(mxt-tsz.cx div 2, y, s);
  //Temperatur
  s:= 'Temp: ' + FloatToStrF(temp, ffFixed, 7,1) + #176+'C';
  tsz:= can.TextExtent(s);
  y:= XY(45);
  can.TextOut(mxt-tsz.cx div 2, y, s);
  //Min-Max-emp
  s:= '[ ' +
  	FloatToStrF(mintemp, ffFixed, 7,1) + ' / ' +
  	FloatToStrF(maxtemp, ffFixed, 7,1) + #176+ 'C ]';
  tsz:= can.TextExtent(s);
  y:= XY(55);
  can.TextOut(mxt-tsz.cx div 2, y, s);
  //Interval
  s:= 'Interval: ' + IntToStr(interval) + 'min';
  y:= XY(85);
  can.TextOut(mxt-tsz.cx div 2, y, s);








  writeln('Dauer: ', IntToStr( GetTickCount() - time));

  spng.Clear;
  img.SaveToStream(spng, writer);

  can.Free;
  img.Free;
  writer.Free;

end;
