    
var vs=`
IN mat4 model;
IN mat4 viewp;
IN vec3 ceye;
IN float time;
IN vec4 color;
#ifdef INST
IN float scale;
in vec4 offs;
#endif
out vec3 wpos;
out vec4 vcol;
vec3 ROTY(vec3 v,float a){vec2 t=vec2(sin(a),cos(a));return vec3(t.y*v.x-t.x*v.z,v.y,t.x*v.x+t.y*v.z);}
float PI=${PI};
void main(){
wpos=pos;
vcol=color;
float id=float(gl_InstanceID);
#ifdef INST
wpos=ROTY(wpos,id);
wpos*=offs.w*scale;
wpos+=offs.xyz;
#endif
wpos=(model*vec4(wpos,1.0)).xyz;
gl_PointSize=(sin(float(gl_VertexID))*0.5+1.2);
gl_Position=viewp*vec4(wpos,1.0);
}
`,fs=`
in vec3 wpos;
in vec4 vcol;
IN vec3 ceye;
IN vec3 cfront;
IN float spot;
IN vec3 target;
vec3 scolor=vec3(.04,.06,.07);
vec3 fcolor=vec3(.2,.5,.8);
IN float time;
IN float mat;
IN vec4 emis;
void main(){
vec3 L=vec3(0,1,0);
vec3 E=wpos-ceye;
float dist = length(E);
E/=dist;
#ifdef POINTS
vec3 N=-E;
#else
vec3 N=NORM(cross(dFdx(wpos),dFdy(wpos)));
#endif
vec4 C=vcol;
#ifndef POINTS
vec3 light = fcolor*max(0.0,(wpos.y-44.0)/20.0) * max(0.0,0.5 + 0.5 * (N.y+1.0)/2.0);//sky
float fog = pow(clamp(dist/150.0,0.0,1.0),0.3);
float spotatt = max(0.0,1.0 - clamp(dist/40.0,0.0,2.0));
vec3 F=(cfront+vec3(0.0,-0.1,0.0));
light += vec3(0.6,0.7,0.8)*(dot(N,-F)*0.25+0.75)*smoothstep(0.92,0.99,max(0.0,dot(F,E)))*spotatt*spot;
C.xyz=mix(C.xyz*light,scolor,fog);
C.xyz+=vec3(0.2,0.5,0.3)*pow(clamp(1.0 - length(target-wpos)/15.0,0.0,1.0),2.5);
#else
C.a /= dist*1.2;
#endif
fragColor=C+emis;
}
`,fx_fs=`in vec2 _uv;IN S2D tex;IN float R;vec2 barrel(vec2 uv,float v){vec2 st=uv-0.5;float t=atan(st.x,st.y);float r=sqrt(dot(st,st));r*=1.0+v*(r*r);return 0.5+r*vec2(sin(t),cos(t));}
void main(){
    float l=length(_uv-vec2(0.5));
    if(l>R)discard;
    if(l>R*0.98)fragColor=vec4(0.1);
    else{
    float vig=pow(1.0-l/0.45,0.5);
    fragColor=vig*(texture(tex,barrel(_uv,-1.2),2.0)*0.5+texture(tex,barrel(_uv,-1.2),4.0)*0.5+texture(tex,barrel(_uv,-1.2))*1.5);;}
    }
`,
final_fs=`in vec2 _uv;IN S2D tex;IN float D;IN float time;
void main(){
    float l=_uv.y+0.1*round(10.0*sin(_uv.x+time)*cos(_uv.x*0.3+time*1.2))*0.1;
    vec4 C=texture(tex,_uv+vec2(0.0,l<D?sin(time*2.0)*0.01:0.0),l<D?3.0:0.0);
    if(l<D){C.xy*=0.8;C.xyz+=vec3(l-D)*0.2;}
    fragColor=C;;}
`

//array helpers
var DEBUG=0
var i=0,j=0,k=0,ox,oy,oz
ARP.p=ARP.push;//ARP.m=ARP.map;ARP.c=ARP.concat;ARP.rand=function(){return this[RAND(this.length)|0]}

SIM=(p,t,s)=>{
    l=p.length/3
    for(var i=0,l2=t.length;i<l2;i+=3)t.p(t[i]+l,t[i+2]+l,t[i+1]+l)
    p.p(...p.map((a,i)=>a*s[i%3]))
}
FETCH=(u,f)=>fetch(u).then((r)=>r.arrayBuffer()).then(f)
LOAD=(url,X,Y,Z,f)=>FETCH(url+".xbin",(a)=>{
        var dataI=new Int8Array(a),dataU=new UINT8(a)
        var pos=[],tris=0,l=dataU[0]
        for(var i=1;i<=l*3;i+=3)
            pos.p(dataI[i]/127,dataU[i+2]/255-0.5,dataI[i+1]/127)
        tris=Array.from(dataU.subarray(l*3+2))
        if(X)SIM(pos,tris,[-1,1,1])
        //if(Y)SIM(pos,tris,[1,-1,1])//not used in this game
        //if(Z)SIM(pos,tris,[1,1,-1])
        var m=meshes[url]=MESH({pos,tris})
        //if(f)f(m,url)
    })

//palette
var fcolor=[.2,.5,.8,1],scolor=[.04,.06,.07,1],fbo,fboView,fboSonar
var NOW=0,PREV=0,GTIME=0,STIME=5,V4=[0,0,0,0]

INPUT(C)
INIT(C)
sh=PROGRAM(vs,fs).uniforms({emis:V4})
sh_inst=PROGRAM(vs,fs,["INST"])
sh_points=PROGRAM(vs,fs,["POINTS"])
//check color alpha
sh_sonar=PROGRAM("IN float time;IN vec3 ceye;IN mat4 viewp;IN mat4 model;out vec2 dist;void main(){vec4 p=model*vec4(pos,1.0);gl_Position=viewp*p;dist=vec2(length(p.xyz-ceye),sin(float(gl_VertexID)+time*10.0)*0.5+0.5);gl_PointSize=3.0;;}","in vec2 dist;IN vec4 color;void main(){fragColor=color*dist.y;;}")
sh_fx=PROGRAM(quad_vs,fx_fs);
sh_final=PROGRAM(quad_vs,final_fs);
meshes.plane=PLANE()
//meshes.cube=CUBE(.45,.5);
LOAD("head",1)
NSIN=(a)=>SIN(a*2*PI)

var SEED=30;
HASH3D=(x,y,z)=>{x=50*FRACT(x*0.3183099+0.71);y=50*FRACT(y*0.3183099+0.113);z=50*FRACT(z*0.3183099-0.247);return -1+2*FRACT(1.375986*SEED+x*y*z*(x+y+z));}
NOISE3D=(x,y,z)=>{let ix=FLOOR(x),iy=FLOOR(y),iz=FLOOR(z);let fx=FRACT(x),fy=FRACT(y),fz=FRACT(z);let ux=fx*fx*(3-2*fx);return LERP( LERP(LERP(HASH3D(ix,iy,iz),HASH3D(ix+1,iy,iz),ux),LERP(HASH3D(ix,iy+1,iz),HASH3D(ix+1,iy+1,iz),ux),fy*fy*(3-2*fy)), LERP(LERP(HASH3D(ix,iy,iz+1),HASH3D(ix+1,iy,iz+1),ux),LERP(HASH3D(ix,iy+1,iz+1),HASH3D(ix+1,iy+1,iz+1),ux),fy*fy*(3-2*fy)), fz*fz*(3-2*fz))}

//WORLD GENERATION
var WSIZE=512,BSIZE=64,HSIZE=BSIZE+20,MAPDOTS=[],WORLD=0,BLOCKS=[]
GENWORLD=()=>{
GEN=(x,y,z)=>!y||!x||!z||x==WSIZE-1||z==WSIZE-1||(NOISE3D(x*0.1,y*0.1-20,z*0.1)+ABS((x-WSIZE/2)*0.005)-1.3*SAT(1-LEN([224,4,348])/30))>0?1:0;
WORLD=new UINT8(WSIZE*WSIZE*HSIZE)
var WORLD2=new UINT8(WORLD);
GETW=(x,y,z)=>y>=HSIZE?0:WORLD[(x|0)+(y|0)*WSIZE+(z|0)*(WSIZE*HSIZE)]
SETW=(x,y,z,v)=>WORLD[x+y*WSIZE+z*(WSIZE*HSIZE)]=v
//HOLE=(x,y,z,v,n=1)=>{for(ox=x-n;ox<=x+n;ox++)for(oy=y-n;oy<=y+n;oy++)for(oz=z-n;oz<=z+n;oz++)SETW(ox,oy,oz,v)}
SETW2=(x,y,z,v)=>WORLD2[x+y*WSIZE+z*(WSIZE*HSIZE)]=v
for(x=0;x<WSIZE;x++)
for(z=0;z<WSIZE;z++) {
    let off=(NOISE3D(x*0.02,0,z*0.03)+1)*5
    for(y=0;y<HSIZE-off;y++)
        if(SETW(x,y,z, GEN(x,y,z)))
        if(RAND()<0.3) MAPDOTS.push(x,y,z)//(x+RAND(1,-0.5),y+RAND(1,-0.5),z+RAND(1,-0.5))
}
//fill
var EMPTY=new UINT8(WORLD.length)

//MARK CELLS SURROUNDED
WORLD2.set(WORLD)
for(x=1;x<WSIZE-1;x++)
for(y=1;y<HSIZE-1;y++)
for(z=1;z<WSIZE-1;z++)
    if(GETW(x,y,z)&&GETW(x-1,y,z)&&GETW(x+1,y,z)&&GETW(x,y-1,z)&&GETW(x,y+1,z)&&GETW(x,y,z-1)&&GETW(x,y,z+1))
        SETW2(x,y,z,2)
WORLD.set(WORLD2)
//SPLIT IN BLOCKS
CREATEBLOCK = (ox,oz)=>{
    var insts = new F32(BSIZE*BSIZE*HSIZE*4),num=0
    for(x=0;x<BSIZE;x++)
    for(y=0;y<HSIZE;y++)
    for(z=0;z<BSIZE;z++)
    {
        if(GETW(x+ox,y,z+oz)==1){
        insts.set([x+ox+RAND(0),y+RAND(0,-1),z+oz+RAND(0),2.2+RAND()],num*4)
        num++}
    }
    BLOCKS.push({num,pos:[ox+BSIZE/2,HSIZE/2,oz+BSIZE/2],insts:BUFFER(insts.subarray(0,num*4),4)})
}
GSIZE=(WSIZE/BSIZE)
for(i=0;i<(GSIZE*GSIZE);i++)
    CREATEBLOCK((i%GSIZE)*BSIZE,FLOOR(i/GSIZE)*BSIZE)
MAPDOTS=BUFFER(MAPDOTS,3)
}

PBUFFER=BUFFER(new F32(256*3),3)//floating particles
PBUFFER.map(()=>RAND(8))
DBUFFER=BUFFER(new F32(10240*3),3,35048)//damage water
var DMG=[],ADDPART=(p)=>DMG.length<10240?DMG.push({p,v:SUB(V3(),[RAND(0.2,0.4),RAND(-0.2),0],p),t:3}):0

//level //[279,19,313],[336,8,468],
var WPs=[[234,20,362],[237,32,360],[232,41,360],[240,50,359],[264,48,360],[277,40,361],[288,40,369],[288,51,369],[297,60,376],[290,73,391],[272,93,421]]
var start=[WSIZE/2,HSIZE*1.5,WSIZE/4],targets=[[223,20,368]],TRG=V3(),TRGdist=1000,sonarbeep=0,out=0,WPBUFFER=BUFFER(WPs.flat(),3)

//player 
var HMODEL=M4(),AMODEL=M4(),UItex=0,isInside=0,signal=0.1,totarget=V3(),spark=0,nrg=0,batt=0,shake=0,PLY=0
RESET=()=>{
    PLY={pos:V3(start),ang:0,nrg:1,spot:2,sonar:100,scan:1,hasTRG:0,vel:V3(),dmg:0.1,open:0,life:1.1,batt:1,speed:[0,0,0]},P=PLY.pos
    TRG=V3(targets[0]);DIV.style.fontSize=""
}
RESET()

//snake
var SNK={pos:V3(WPs[0]),index:0,ang:0,call:4,dist:10000,tail:0}
SNK.tail=BUFFER(new F32(50*4),4)
SNK.part=(v)=>SNK.tail.d.subarray(v*4,v*4+3)
for(i=1;i<50;++i)SNK.tail.d.set([SNK.pos[0]+i,SNK.pos[1],SNK.pos[2]+5,LERP(1.5,0.2,i/50)],i*4)
SNK.tail.update();

UIctx=document.createElement("canvas").getContext("2d")//for UI
var INRANGE=(v,min,max)=>v>=min&&v<max

//audio stuff
var ACX,ADEST,AVOL,asea,abeep,asnk,asonar,ashake

CHANNEL=()=>{
    var osc=ACX.createOscillator(),noise=ACX.createBufferSource(),filter=ACX.createBiquadFilter(),gain=ACX.createGain()
    osc.frequency.value=0;noise.buffer=BUF;noise.loop=true;filter.frequency.value=10000;gain.gain.value=0;
    osc.connect(gain);noise.connect(filter);filter.connect(gain);gain.connect(AVOL)
    var ch={osc,noise,filter,gain};
    ch.volume=(v)=>gain.gain.linearRampToValueAtTime(SAT(v),ACX.currentTime+0.1)
    ch.freq=(v)=>osc.frequency.value=v
    return ch;
}

STARTAUDIO=()=>{
    ACX=new AudioContext();ADEST=ACX.destination;AVOL=ACX.createGain()
    AVOL.gain.value=.5,AVOL.connect(ADEST)//main volume
    //create white noise
    var s=2*ACX.sampleRate;BUF=ACX.createBuffer(1,s,ACX.sampleRate),data=BUF.getChannelData(0);
    for (var i=0;i<s;i++)data[i]=RANDOM()*2-1;
    asea=CHANNEL(),abeep=CHANNEL(),asnk=CHANNEL(),asonar=CHANNEL(),ashake=CHANNEL()
    asea.noise.start()//copter sound
    asea.filter.Q.value=15
    abeep.osc.start()//scanner sound
    asnk.osc.start()
    //asnk.noise.start()//copter sound
    asonar.noise.start()//copter sound
    //asonar.osc.start()
    ashake.osc.start()
}

let gradient;

//main loop
var eye=V3(),target=V3(),front=V3(),IMP=V3()
CAM_EYE.set([180,5,-250])
loop=()=>{
    if(!WORLD)GENWORLD()
    requestAnimationFrame(loop)
    NOW=TIME()
    var dt=MIN(0.1,NOW-PREV);PREV=NOW
    var W=C.width=BODY.offsetWidth,H=C.height=BODY.offsetHeight,_W=W>>2,_H=H>>2
    
    if(!fbo||fbo.h!=_H||fbo.w!=_W)fbo=FBO(TEX(_W,_H).param(0,NEAREST).param(1,9984))
    if(!fboView||fboView.h!=_H)fboView=FBO(TEX(_H,_H).param(0,NEAREST).param(1,9987))
    if(!fboSonar||fboSonar.tex.h!=(_H)|0)fboSonar=FBO(TEX(_H,_H).param(0,NEAREST))
    GTIME+=dt;STIME-=dt

    sonarbeep=RAND()<(0.03+0.5*SAT(1-TRGdist/100))?1:0
    out=P[1]>HSIZE*2?1:0

    let m="Explore the depths, find the origin of the signal "+(ACX?"":"(Click for audio)")
    if(PLY.won)
        m="Mission Accomplished, Time: " + (PLY.time|0) + "s"
    else if(PLY.life<=0)
        m="You are dead, press SPACE to restart"
    else if(PLY.open>0)
        m="FIX LEAK, press G multiple times!!"
    else if(PLY.hasTRG)
        m="Carry it to the surface!"
    DIV.innerText=m

    //audio
    if(ACX){
        asea.filter.frequency.value=(PLY.open>0?400:0)+out?1600:100+P[1]+ABS(PLY.speed[2])
        asea.volume(out?SIN(GTIME)*0.3+0.4:0.9)
        abeep.volume(PLY.scan*signal/2)
        abeep.freq(440*sonarbeep)
        let f=FRACT(GTIME/SNK.call+RAND()*0.1)
        asnk.volume((1-f)-SAT(SNK.dist/50))
        asnk.freq(120-50*POW(f,0.15)+0*f*SIN(GTIME*10*f));
        f=FRACT(GTIME/2)
        asonar.volume(PLY.sonar?0.1*POW(1-f,2.0):0)
        asonar.filter.frequency.value=800-f*200
        asonar.filter.Q.value=30+RAND()*5
        ashake.volume(shake*0.5)
        ashake.freq(RAND(140,50))
    }

    //Update UI texture ************************
    let ctx=UIctx,UIC=ctx.canvas,dsize=_H/7,ang;
    ctx.imageSmoothingEnabled=false
    UIC.width=_W;UIC.height=_H
    ctx.textAlign="center";ctx.font=(dsize*0.2|0)+"px monospace"
    ctx.translate(0,_H);ctx.scale(1,-1)//reverse UI
    
    FCOLOR=(v)=>ctx.strokeStyle=ctx.fillStyle=v
    RECT=(x,y,w,h,c)=>{FCOLOR(c);ctx.fillRect(x,y,w,h)}
    CIRCLE=(x,y,r,c)=>{c?FCOLOR(c):0;ctx.beginPath();ctx.arc(x,y,r,0,2*PI);ctx.fill()}
    DIAL=(msg,x,y,r,n,v,s=5,t=0)=>{
        CIRCLE(x,y,r+2,"#333")
        CIRCLE(x,y,r,"#000")
        ctx.save();ctx.translate(x,y);FCOLOR("#4A6");ctx.fillText(msg||"",0,r/1.5)
        ctx.rotate(PI/2-(t==2?v*2*PI-PI/2:0.95))
        ctx.save();FCOLOR("#620")
        if(t==1){ctx.beginPath();ctx.moveTo(0,0);ctx.arc(0,0,r*0.9,0,0.3);ctx.fill();CIRCLE(0,0,r*0.35,"#000")}
        
        FCOLOR("#0A4")
        for(let i=0;i<n+1;++i){
            ctx.fillRect(-1,r*0.8,i%s?0.5:2,4)
            if(i%s==0&&t!=3)ctx.fillText(t==2?("ENESW"[i/s]):i,0,r*0.7)
            ctx.rotate(2*PI*(t==2?1:0.8)/n)
        }
        ctx.restore()
        ctx.rotate(SAT(v)*2*PI*(t==2?1:0.8))
        FCOLOR("#4C8")
        ctx.fillRect(-1,0,2,r*0.9)
        CIRCLE(0,0,r*0.05)
        ctx.restore()
    }
    RECT(0,0,_W,_H,"#111")//bg
    ctx.save();ctx.translate(_W/2-_H/1.5,0);
    for(i=0;i<4;++i)
        RECT(_H*-[0.12,0.06,0.04,0.02][i],0,_H*0.2/(i+1),_H,["#000","#222","#333","#444"][i])//pipe
    DIAL("BATT",0,_H-dsize*1.1,dsize,30,batt)//battery
    let press=1-SAT(P[1]/170)
    DIAL("ATM",0,dsize+10,dsize,14,press+RAND(0.2*SAT(press-0.8)),1,1)//pressure
    //gear
    ctx.strokeStyle="#371911";ctx.lineWidth=3
    ctx.translate(0,_H/2);ctx.rotate(PLY.open)
    ctx.beginPath();
    for(i=0;i<8;++i){ctx.rotate(PI*2/8);ctx.moveTo(0,0);ctx.lineTo(-8.5,19);ctx.lineTo(7,20)}
    ctx.stroke();
    CIRCLE(0,0,3,"#111")
    ctx.restore();

    ang=(PLY.ang/(2*PI))%1
    DIAL(0,_W/2+_H/1.3-dsize,_H/1.8,dsize*0.75,8,1-(ang+(ang<0?1:0)),2,2) //compass
    DIAL("TANK",_W/2-_H/2.25,_H*0.1,dsize*0.5,2,PLY.speed[1]/32+0.5,0.5,3) //elevation
    nrg=LERP(PLY.nrg/20,nrg,0.99)
    DIAL("KWh",_W/2-_H/2.32,_H*0.9,dsize*0.5,15,nrg+SIN(GTIME)*0.02,15,3) //energy
    DIAL("AIR",_W/2-_H/1.8,_H*0.65,dsize*0.4,15,PLY.life,15,3) //air

    //scanner
    ctx.save();
    ctx.translate(_W/2+_H/1.7,_H-dsize*1.1)
    CIRCLE(0,0,dsize+2,"#222")
    CIRCLE(0,0,dsize,"#000")
    if(PLY.scan){
    ctx.strokeStyle="#0F0";ctx.beginPath();ctx.moveTo(0,0);
    for(i=0;i<dsize*1.5;++i)ctx.lineTo(i-dsize*0.75,(RAND(4,-2)+sonarbeep*signal*RAND(dsize,-dsize/2)+spark*RAND(30,-15))*NSIN(i/dsize*0.34))
    ctx.stroke();
    ctx.restore();
    }

    if(!UItex||UItex.w!=_W||UItex.h!=_H)UItex=TEX(_W,_H,UIC)
    else UItex.update(UIC)
    UItex.param(0,9728)
    //********************* */


    //render
    INIT(C)

    //main view
    fboView.bind(1)
    CLEAR(scolor,1)
    front[0]=SIN(PLY.ang);front[2]=COS(PLY.ang)
    ADD(eye,P,[SIN(GTIME)*0.1,RAND(0.1)*isInside,COS(GTIME*0.97)*0.1])
    ADD(target,eye,front)
    for(i=0;i<3;++i)target[i]+=RAND(shake)
    //target[1]-=MOUSE.pos[1]/W-0.5
    CAMERA(eye,target,UP,80,1,0.1,150)

    GLSET(ZTEST)
    GLSET(CULL,0)
    GLSET(BLEND,0)

    //sea bed
    DRAW("plane",sh,{emis:V4,spot:0,target:[0,-1e3,0],color:[0,0,0,1],model:TRS(HMODEL,[eye[0],HSIZE*2,eye[2]],0,1000)})
    GLSET(CULL,1)
    //sea floor
    for(b of BLOCKS)
        if(CAMTESTBOX(b.pos,[BSIZE,HSIZE,BSIZE]))
            DRAW("head",sh_inst,{color:[0.5,0.6,0.7,0.1],target:TRG,scale:1,emis:V4,spot:PLY.spot,model:AMODEL},4,b.num,{offs:b.insts})
    if(PLY.hasTRG)ADD(TRG,eye,[front[0],front[1]-0.7,front[2]],0.75)
    DRAW("head",sh,{color:[0.1,0.1,0.1,1],emis:[0.2,0.5,0.3,0],spot:PLY.spot*2,model:TRS(HMODEL,TRG,0,1)})//Target
    BLENDFUNC("A")
    for(i=0;i<4;++i)//SNAKE
      DRAW("head",sh_inst,{color:[0.1,0.1,0.1,0.2],scale:0.5+i/4,emis:(!i&&spark)?[0,1,3,0]:[-0.02,-0.02,-0.02,0],spot:PLY.spot*2,model:AMODEL},5,50,{offs:SNK.tail})//snake
    if(!out) for(i=-2;i<=2;++i)for(j=-1;j<=1;++j)for(k=-2;k<=2;++k) //dust
        DRAW({pos:PBUFFER,0:256},sh_points,{color:[0.7,0.8,0.9,0.3],emis:V4,model:TRS(HMODEL,[ROUND(P[0]/8+i)*8,ROUND(P[1]/8+j)*8,ROUND(P[2]/8+k)*8],0,1)},0)
    //if(DEBUG&&WPs.length)DRAW({pos:WPBUFFER,0:WPs.length},0,{color:[1,1,1,1],model:AMODEL},3)//DEBUG line
        
    //landscape
    sh.uniforms({time:GTIME})

    GLSET(ZTEST,0)
    GLSET(BLEND,0)

    //end main view
    fboView.bind(0)
    fboView.tex.mips()

    //sonar
    fboSonar.bind(1)
    CLEAR(V4)
    //VIEWPORT(W-420,H-420,400,400)
    ADD(target,P,[-front[2],front[1],front[0]],PLY.sonar)
    CAMERA(target,P,UP,60,1,PLY.sonar-3,PLY.sonar+3)
    GLSET(ZTEST,0)
    BLENDFUNC("B")
    if(PLY.sonar){DRAW({pos:MAPDOTS,0:MAPDOTS.d.length/3},sh_sonar,{model:AMODEL,time:GTIME,ceye:P,color:[0.02,1,0.07,0.4],model:AMODEL},0)//world
        DRAW("head",sh,{color:[0,0,0,1],emis:[0,1,0.1,0],spot:0,model:TRS(HMODEL,ADD(V3(),P,[0,-1,0]),[0,PLY.ang],[8,4,8])},4)//sub
        DRAW("head",sh_inst,{color:[0.1,0.1,0.1,0.5],scale:2,emis:[1,0.3,0,0],spot:0,model:AMODEL},5,18,{offs:SNK.tail})//snake
        DRAW("head",sh,{emis:[0.1,0.5,1,0],model:TRS(HMODEL,TRG,0,2)})//Target
    }
    GLSET(CULL,0)
    //DRAW("plane",sh,{emis:[0,1,0,0],spot:0,target:[0,-1e3,0],color:[0,0,0],model:TRS(HMODEL,[eye[0],HSIZE*2,eye[2]],0,1000)})
    //DRAW("plane",sh_sonar,{time:GTIME,ceye:P,sonar:PLY.sonar/2,color:[0.01,1,0.07,1],model:TRS(HMODEL,[eye[0],HSIZE*2,eye[2]],0,1000)})
    fboSonar.bind(0)
    fboSonar.tex.mips()

    //final frame
    fbo.bind(1)
    //UI
    GLSET(BLEND,1)
    BLENDFUNC("A")
    UItex.toVP();

    //add main view
    GLSET(BLEND,0)
    VIEWPORT(_W/2-_H/2,0,_H,_H) 
    fboView.tex.toVP(sh_fx,{R:0.5})

    //add sonar view
    VIEWPORT(_W/2+_H/2.3,_H/1.8,_H/2.2,_H/2.2)
    fboSonar.tex.toVP(sh_fx,{R:0.5})

    //damage water
    if(DMG.length){
    VIEWPORT(0,0,_W,_H)
    DBUFFER.update(DMG.map(a=>(a.t-=dt,a.v[1]-=dt*0.5,ADD(a.p,a.p,a.v,dt*0.5),a.p)).flat())
    DMG=DMG.filter(a=>a.t>0&&(a.p[1]>PLY.dmg-0.55-RAND(0.2)))//remove dead ones
    CAMERA([0,0,1],[0,0,0],UP,45,1,0,10)
    BLENDFUNC("A")
    DRAW({pos:DBUFFER,0:DMG.length},0,{color:[0.5,0.55,0.6,0.2],psize:5,model:AMODEL},0)
    }
    if(PLY.open>0){
        PLY.dmg+=dt*0.01//water filling
        for(i=0,l=RAND(10,10)*PLY.open;i<l;++i)ADDPART([-0.3+RAND(0.01),RAND(0.01)+0.4,0])
    }

    fbo.bind(0)
    fbo.tex.mips()
    GLSET(BLEND,0)
    VIEWPORT(0,0,W,H)
    fbo.tex.toVP(sh_final,{time:GTIME,D:PLY.dmg*1.5-0.3})



    //update
    /*
    camS=.01//?.005:.01
    if(DOC.pointerLockElement)
    {
        //camA-=MOUSE.delta[0]*-camS*.4//MOUSE.buttons?MOUSE.delta[0]*0.03:0
        //camPitch+=MOUSE.delta[1]*(MOUSE.buttons?0.01:0)
        //camY=CLAMP(camY+MOUSE.delta[1]*camS,-PI/4,PI/4)
        //camZ=CLAMP(camZ+MOUSE.wheel*0.01,1,10)
    }
    */

    prevpos=V3(P)

    /*
    if(DEBUG){
        if(KEYS["W"])ADD(P,P,front,dt*50.5)
        if(KEYS["S"])ADD(P,P,front,-dt*50.5)
        if(KEYS["E"])PLY.pos[1]+=dt*20.5
        if(KEYS["Q"])PLY.pos[1]-=dt*20.5
        if(KEYS["A"])PLY.ang+=dt*0.75
        if(KEYS["D"])PLY.ang-=dt*0.75
        if(KEYSP["F"])PLY.spot=PLY.spot?0:2
        if(KEYSP["Space"]){ WPs.push([P[0]|0,P[1]|0,P[2]|0]); WPBUFFER=BUFFER(WPs.flat(),3)}
        if(KEYSP["Backspace"]){WPs.pop();WPBUFFER=BUFFER(WPs.flat(),3)}
        if(KEYSP["P"])P.set(TRG)
    }
    else*/
    if(!PLY.won&&PLY.life>0&&PLY.batt>0)
    {
        if(KEYS["W"])PLY.speed[2]+=dt*25
        if(KEYS["S"])PLY.speed[2]-=dt*25
        if(KEYS["E"])PLY.speed[1]+=dt*10.5
        if(KEYS["Q"])PLY.speed[1]-=dt*10.5
        if(KEYS["A"])PLY.speed[0]+=dt*1.5
        if(KEYS["D"])PLY.speed[0]-=dt*1.5
        //if(KEYS["E"])PLY.pos[1]+=dt*10.5
        //if(KEYS["Q"])PLY.pos[1]-=dt*10.5
        if(KEYSP["F"])PLY.spot=PLY.spot?0:2
        if(KEYSP["G"]&&PLY.open>0){PLY.open-=0.05}
        if(KEYSP["R"]&&PLY.hasTRG){PLY.hasTRG=0;ADD(TRG,P,front,2.5)}
        if(KEYSP["Digit1"]){PLY.sonar=!PLY.sonar?50:(PLY.sonar==50?100:0)}
        if(KEYSP["Digit2"])PLY.scan=PLY.scan?0:1
        if(KEYS["Space"])SCALE(PLY.speed,PLY.speed,0.99)//break
    }
    if(KEYSP["Space"]&&(PLY.life<=0||PLY.won))RESET()
    //if(KEYSP["Home"])DEBUG=!DEBUG

    PLY.ang+=dt*PLY.speed[0]*0.2
    ADD(P,P,PLY.vel,dt)
    SCALE(IMP,front,PLY.speed[2]);
    ADD(IMP,IMP,UP,PLY.speed[1]);
    ADD(PLY.vel,PLY.vel,IMP,dt);
    SCALE(PLY.vel,PLY.vel,0.95);
    PLY.speed[0] *= 0.99;
    PLY.speed[2] *= 0.99;
    PLY.speed[1]=CLAMP(PLY.speed[1],-16,16)

    PLY.batt -= dt*(PLY.nrg=4+PLY.sonar/50+PLY.spot*2+PLY.speed[2]/10+PLY.scan*2)*0.0001;
    if(PLY.batt<=0){PLY.life-=dt*0.02;PLY.spot=0;PLY.sonar=0;PLY.nrg=0}
    

    //COLLISIONS
    isInside=0
    if(INRANGE(P[0],0,WSIZE)&&INRANGE(P[1],0,HSIZE)&&INRANGE(P[2],0,WSIZE))
    //for(i=-1;i<=1;++i)for(j=-1;j<=1;++j)for(k=-1;k<=1;++k)
        isInside|=GETW(P[0],P[1],P[2])
    if(isInside&&!DEBUG){
        //console.log(LEN(PLY.vel))
        if(RAND()<0.1)PLY.open=1
        PLY.pos.set(prevpos)
        SCALE(PLY.vel,PLY.vel,-0.8)
        PLY.speed.fill(0)
        shake+=1
    }
    for(i=0;i<3;++i)P[i]=CLAMP(P[i],[5,1,5][i],[WSIZE-5,HSIZE*2+2,WSIZE-5][i])//avoid scaping

    //TARGET
    SUB(totarget,TRG,P);totarget[1]=0;NORM(totarget,totarget)
    PLY.hasTRG=(TRGdist=DIST(TRG,P))<1.2
    signal=SAT(1-DIST(P,TRG)*0.001)*POW(SAT(DOT(front,totarget)),4)

    //SNAKE
    SNK.dist=MIN(DIST(P,SNK.pos),DIST(P,SNK.part(10)),DIST(P,SNK.part(25)))//dist to player
    let tail=SNK.tail.d,prev=SNK.pos,curr=WPs[SNK.index]
    if(!curr){WPs.reverse();curr=WPs[0];SNK.index=0}//backtrack
    spark=RAND()<0.008 //snake spark
    let d=DIST(tail,curr)
    SUB(totarget,curr,prev)
    NORM(totarget,totarget)
    ADD(prev,prev,totarget,dt*5)//snake head movement
    if(DIST(prev,curr)<2)SNK.index+=1//next WP)
    SNK.part(0).set(prev)
    //tail.set([WSIZE/2+SIN(GTIME*0.1)*20,HSIZE + 10 + SIN(GTIME*0.4)*2,WSIZE/2+COS(GTIME*0.1)*20])
    for(i=1;i<50;++i){
        prev=ADD(V3(),SNK.part(i-1),[SIN(GTIME*3+i/5)/2,0,0],((i+1)/550))
        curr=SNK.part(i)
        d=DIST(prev,curr),mind=(tail[i*4-1]+tail[i*4+3])/4
        SUB(totarget,prev,curr)
        ADD(curr,curr,totarget,(d-mind)/d)
        tail.set(curr,i*4)
    }
    SNK.tail.update();
    if(SNK.dist<10&&spark){PLY.batt-=dt*2;shake+=0.1}
    batt=LERP(batt,PLY.batt,0.01)
    shake*=0.95

    if(!PLY.won&&P[1]>=170&&PLY.hasTRG){PLY.won=1;PLY.time=GTIME;DIV.style.fontSize=120}

    END()

    if(MOUSE.buttons&&!ACX)STARTAUDIO();
}

loop();

/*
ONMOUSE=(e)=>{
    if(e.type=="mousedown") C.requestPointerLock()
}
*/

//</script>