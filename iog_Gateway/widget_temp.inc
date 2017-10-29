//aus der unit sensor ausgelagerte Procedure zum Zeichnen der Level-Gauge
//Ergebnis in spng (MemoryStream) als png ablegen


procedure draw_widget_temp(hSize: integer);

const ratio = 0.75; //width = hSize * ratio

var writer : TFPCustomImageWriter;
    w, h, ang, szval, sz, x, y, mxt, myt, h2, w2: integer;
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
