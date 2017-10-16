//aus der unit sensor ausgelagerte Procedure zum Zeichnen der Level-Gauge
//Ergebnis in spng (MemoryStream) als png ablegen

procedure draw_widget_level(hSize: integer);

const ratio = 1.5; //width = hSize * ratio

var can : TFPCustomCanvas;
    img : TFPCustomImage;
    writer : TFPCustomImageWriter;
    w, h, ang, szval, sz, x, y, mxt: integer;
    mcf, pA, pE, pT: TPoint;
    rsA1, rsA2, rsE, rsT, rsN, a: real;
    rval: TRect;
    tsz: TSize;
    s: string;
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
  FontMgr.SearchPath:='/usr/share/fonts/truetype/droid/'; //dejavu/';
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
  rsT:= XY(39);
  rsN:= XY(29);  //Needle

  can.Pen.FPColor := TColorToFPColor(dsgn.scaleLine);
	can.Pen.Width := XY(1.2);
  can.Font.Size := XY(3.2);
	can.Font.Name:= 'DroidSansMono'; //'DejaVuSansCondensed';
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

		//Prozentwert Skala 0 / 50 / 100
    case ang of   //mit Feinjustage entspr. TextlÃ¤nge
    	0: begin
    		pT.x:= mcf.x - round(rsT*sin(a-0.02*pi));
  			pT.y:= mcf.y + round(rsT*cos(a-0.02*pi));
    		can.Font.Orientation := 10; // round(RadToDeg(a)*(-10))+1800;
    		can.TextOut(pT.x, pT.y, IntToStr(ang)+'%');
    	end;
      50: begin
    		pT.x:= mcf.x - round(rsT*sin(a-0.03*pi));
  			pT.y:= mcf.y + round(rsT*cos(a-0.03*pi));
    		can.Font.Orientation := round(RadToDeg(a)*(-10))+1800;
    		can.TextOut(pT.x, pT.y, IntToStr(ang)+'%');
    	end;
      100: begin
    		pT.x:= mcf.x - round(rsT*sin(a-0.05*pi));
  			pT.y:= mcf.y + round(rsT*cos(a-0.05*pi));
    		can.Font.Orientation := round(RadToDeg(a)*(-10))+1800;
    		can.TextOut(pT.x, pT.y, IntToStr(ang)+'%');
    	end;
    end; //case

  	a:= a + 0.075*pi;
    ang:= ang + 5;
  end;


  writeln('Dauer: ', IntToStr( GetTickCount() - time));

  spng.Clear;
  img.SaveToStream(spng, writer);

  can.Free;
  img.Free;
  writer.Free;

end;
