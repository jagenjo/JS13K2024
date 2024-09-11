//$=a=>{for (const b in a)a[b.replace(/^(..)[a-z]*([A-Z]?[a-z]?)[a-z]*([A-Z]?)\D*(.+)$/,"$1$2$3$4")]=a[b]}
//$(window)
KEYS={},KEYSP={},MOUSE={pos:[0,0],delta:[0,0],wheel:0,buttons:0,prevb:0}
gl=0,DOC=document,BODY=DOC.body,F=0
BODY.onkeydown=BODY.onkeyup=function(e){var v=e.type=="keydown",c=e.code.substr(0,3)=="Key"?e.code.substr(3):e.code;KEYSP[c]=(v&&!KEYS[c]);KEYS[c]=v;if(window.ONKEY)ONKEY(e)};
D2R=0.0174532925,maths=Object.getOwnPropertyNames(Math)
for(var i in maths) window[maths[i].toUpperCase()]=Math[maths[i]]
CLAMP=(a,b,c)=>MIN(MAX(a,b),c)
SAT=(a)=>CLAMP(a,0,1)//saturate
F32=Float32Array,UINT8=Uint8Array,F32P=F32.prototype,IDM4=new F32([1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1]),ARP=Array.prototype;
//F32P.MUL=function(v){return v.length==16?MULTMAT4(M4(),this,v):(this.length==16?MAT4xV3(V3(),this,v):V3([this[0]*v[0],this[1]*v[1],this[2]*v[2]]))}
//F32P.ADD=function(v){return ADD(V3(),this,v)},F32P.SUB=function(v){return SUB(V3(),this,v)}
//F32P.SCALE=function(v){return SCALE(V3(),this,v)}
//F32P.MUL=ARP.MUL=function(v){return this.map(a=>a*v)}
V3=(v)=>new F32(v||3)
M4=(v)=>V3(v||IDM4)
VP_DATA=V3(4),ZERO=V3(),UP=V3([0,1,0])
TIME=()=>performance.now()*.001;
LERP=(a,b,f)=>a*(1-f)+b*f
ILERP=(a,b,v)=>(v-a)/(b-a)
REMAP=(v,l1,h1,l2,h2)=>LERP(l2,h2,ILERP(l1,h1,v))
RAND=(v=1,off=0)=>RANDOM()*v+(off)
FRACT=(i)=>i-FLOOR(i)
ADD=(t,n,r,s=1)=>(t[0]=n[0]+r[0]*s,t[1]=n[1]+r[1]*s,t[2]=n[2]+r[2]*s,t)//add but also scale and add
SUB=(t,n,r)=>(t[0]=n[0]-r[0],t[1]=n[1]-r[1],t[2]=n[2]-r[2],t)
SCALE=(t,n,r)=>{t=t||V3();if(r.length){t[0]=n[0]*r[0],t[1]=n[1]*r[1],t[2]=n[2]*r[2]}else{t[0]=n[0]*r,t[1]=n[1]*r,t[2]=n[2]*r};return t}
DOT=(t,n)=>t[0]*n[0]+t[1]*n[1]+t[2]*n[2]
CROSS=(t,n,r)=>{var a=n[0],e=n[1],u=n[2],o=r[0],i=r[1],s=r[2];return t[0]=e*s-u*i,t[1]=u*o-a*s,t[2]=a*i-e*o,t}
LEN=(t)=>SQRT(t[0]*t[0]+t[1]*t[1]+t[2]*t[2]);
ROTY=(o,v,a)=>{o=o||V3();var c=COS(a),s=SIN(a),x=v[0],z=v[2];o[0]=c*x-s*z;o[1]=v[1];o[2]=s*x+c*z;return o}
LERPV3=(o,a,b,f)=>{for(var i=0;i<o.length;++i)o[i]=a[i]*(1-f)+b[i]*f;return o}
NORM=(t,n)=>{var r=n[0],a=n[1],e=n[2],u=r*r+a*a+e*e;return u>0&&(u=1/SQRT(u),t[0]=n[0]*u,t[1]=n[1]*u,t[2]=n[2]*u),t}
DIST=(t,n)=>{var r=n[0]-t[0],a=n[1]-t[1],e=n[2]-t[2];return SQRT(r*r+a*a+e*e)}
TMAT4=(t,n)=>{t.set(IDM4);return t[12]=n[0],t[13]=n[1],t[14]=n[2],t}
RMAT4=(t,n,r)=>{t.set(IDM4);var e=r[0],u=r[1],o=r[2],i=SQRT(e*e+u*u+o*o),s=0,c=0,f=0;if(i<0.0001)return null;return e*=i=1/i,u*=i,o*=i,s=SIN(n),c=COS(n),f=1-c,t[0]=e*e*f+c,t[1]=u*e*f+o*s,t[2]=o*e*f-u*s,t[4]=e*u*f-o*s,t[5]=u*u*f+c,t[6]=o*u*f+e*s,t[8]=e*o*f+u*s,t[9]=u*o*f-e*s,t[10]=o*o*f+c,t}
SMAT4=(t,n)=>{t=t||M4();t.set(IDM4);if(n.toFixed)n=[n,n,n];return t[0]=n[0],t[5]=n[1],t[10]=n[2],t}
APPLYTRANS=(m,n,v)=>{var t=TMAT4(M4(),v);return MULTMAT4(m||t,n,t)}
APPLYROT=(m,n,a,v)=>{var r=RMAT4(M4(),a,v);return MULTMAT4(m||r,n,r)}
APPLYSCALE=(m,n,v)=>{var s=SMAT4(M4(),v);return MULTMAT4(m||s,n,s)}
COLOR=(v,a=255)=>[v>>16&255,v>>8&255,v&255,a].MUL(1/255)
TRS=(m,t,r,s,M)=>{m=m||M4();var T=TMAT4(M4(),t);var R=M4();if(r){if(r[0])APPLYROT(R,R,r[0],[1,0,0]);if(r[1])APPLYROT(R,R,r[1],[0,1,0]);if(r[2])APPLYROT(R,R,r[2],[0,0,1]);}var S=SMAT4(M4(),s);MULTMAT4(m,M||M4(),T);MULTMAT4(m,m,R);return MULTMAT4(m,m,S)}
MULTMAT4=(t,n,r)=>{var a=n[0],e=n[1],u=n[2],o=n[3],i=n[4],s=n[5],c=n[6],f=n[7],M=n[8],h=n[9],l=n[10],v=n[11],d=n[12],b=n[13],m=n[14],p=n[15],P=r[0],A=r[1],E=r[2],O=r[3];return t[0]=P*a+A*i+E*M+O*d,t[1]=P*e+A*s+E*h+O*b,t[2]=P*u+A*c+E*l+O*m,t[3]=P*o+A*f+E*v+O*p,P=r[4],A=r[5],E=r[6],O=r[7],t[4]=P*a+A*i+E*M+O*d,t[5]=P*e+A*s+E*h+O*b,t[6]=P*u+A*c+E*l+O*m,t[7]=P*o+A*f+E*v+O*p,P=r[8],A=r[9],E=r[10],O=r[11],t[8]=P*a+A*i+E*M+O*d,t[9]=P*e+A*s+E*h+O*b,t[10]=P*u+A*c+E*l+O*m,t[11]=P*o+A*f+E*v+O*p,P=r[12],A=r[13],E=r[14],O=r[15],t[12]=P*a+A*i+E*M+O*d,t[13]=P*e+A*s+E*h+O*b,t[14]=P*u+A*c+E*l+O*m,t[15]=P*o+A*f+E*v+O*p,t}
LOOKAT=(t,n,r,u)=>{t.set(IDM4);var o=0,i=0,s=0,c=0,f=0,M=0,h=0,l=0,v=0,d=0,b=n[0],m=n[1],p=n[2],P=u[0],A=u[1],E=u[2],O=r[0],R=r[1],y=r[2];if(ABS(b-O)<0.0001&&ABS(m-R)<0.0001&&ABS(p-y)<0.0001)return t;h=b-O,l=m-R,v=p-y,d=1/SQRT(h*h+l*l+v*v),o=A*(v*=d)-E*(l*=d),i=E*(h*=d)-P*v,s=P*l-A*h,(d=SQRT(o*o+i*i+s*s))?(o*=d=1/d,i*=d,s*=d):(o=0,i=0,s=0);c=l*s-v*i,f=v*o-h*s,M=h*i-l*o,(d=SQRT(c*c+f*f+M*M))?(c*=d=1/d,f*=d,M*=d):(c=0,f=0,M=0);return t[0]=o,t[1]=c,t[2]=h,t[4]=i,t[5]=f,t[6]=l,t[8]=s,t[9]=M,t[10]=v,t[12]=-(o*b+i*m+s*p),t[13]=-(c*b+f*m+M*p),t[14]=-(h*b+l*m+v*p),t}
PERSP=(t,n,r,a,e)=>{var u=1/TAN((n*D2R)/2),o=0;t[0]=u/r,t[1]=0,t[2]=0,t[3]=0,t[4]=0,t[5]=u,t[6]=0,t[7]=0,t[8]=0,t[9]=0,t[11]=-1,t[12]=0,t[13]=0,t[15]=0,null!=e&&e!==1/0?(o=1/(a-e),t[10]=(e+a)*o,t[14]=2*e*a*o):(t[10]=-1,t[14]=-2*a);return t}
ORTHO=(t,n,r,a,e,u,o)=>{var i=1/(n-r),s=1/(a-e),c=1/(u-o);return t[0]=-2*i,t[5]=-2*s,t[10]=2*c,t[12]=(n+r)*i,t[13]=(e+a)*s,t[14]=(o+u)*c,t}
TRANS3D=(t,n,r)=>{var a=n[0],e=n[1],u=n[2],o=r[3]*a+r[7]*e+r[11]*u+r[15];return o=o||1,t[0]=(r[0]*a+r[4]*e+r[8]*u+r[12])/o,t[1]=(r[1]*a+r[5]*e+r[9]*u+r[13])/o,t[2]=(r[2]*a+r[6]*e+r[10]*u+r[14])/o,t}
MAT4xV3=(out,m,a,w=1)=>{var x=a[0],y=a[1],z=a[2];out[0]=m[0]*x+m[4]*y+m[8]*z+m[12]*w;out[1]=m[1]*x+m[5]*y+m[9]*z+m[13]*w;out[2]=m[2]*x+m[6]*y+m[10]*z+m[14]*w;return out}
VIEW_M4=M4(),PROJ_M4=M4(),VP_M4=M4(),MVP_M4=M4(),/*FPLANES=0,*/CAM_EYE=V3(),CAM_CENTER=V3(),CAM_FRONT=V3(),CAM_RIGHT=V3(),CAM_TOP=V3();
CAMERA=(eye,center,up,fov,a,near=.1,far=1000,ortho=0)=>{CAM_EYE.set(eye);CAM_CENTER.set(center);SUB(CAM_FRONT,CAM_CENTER,CAM_EYE);NORM(CAM_FRONT,CAM_FRONT);CROSS(CAM_RIGHT,CAM_FRONT,up);NORM(CAM_RIGHT,CAM_RIGHT);LOOKAT(VIEW_M4,eye,center,up);ortho?ORTHO(PROJ_M4,-fov*a,fov*a,-fov,fov,near,far):PERSP(PROJ_M4,fov,a,near,far);MULTMAT4(VP_M4,PROJ_M4,VIEW_M4);EXTRACTPLANES(VP_M4)}
INIT=(C)=>{if(!gl){gl=C.getContext("webgl2",{});_INITGL()}VIEWPORT(0,0,C.width,C.height)}
INPUT=(C)=>{C.oncontextmenu=(e)=>false;C.onmousedown=C.onmouseup=C.onmousemove=(e)=>{e.preventDefault();MOUSE.delta[0]=e.pageX-MOUSE.pos[0]+e.movementX,MOUSE.delta[1]=e.pageY-MOUSE.pos[1]+e.movementY,MOUSE.pos[0]=e.pageX,MOUSE.pos[1]=e.pageY;MOUSE.buttons=e.buttons;if(window.ONMOUSE)ONMOUSE(e)};BODY.onwheel=(e)=>MOUSE.wheel=e.deltaY}
END=()=>{MOUSE.delta.fill(0);KEYSP={};MOUSE.wheel=0;MOUSE.prevb=MOUSE.buttons}
VIEWPORT=(x,y,w,h)=>{VP_DATA.set([x,y,w,h]);gl.viewport(x,y,w,h)}

//enable if yhou need them
//PROJ3D=(out,m,a)=>{var ix=a[0],iy=a[1],iz=a[2],ox=m[0]*ix+m[4]*iy+m[8]*iz+m[12],oy=m[1]*ix+m[5]*iy+m[9]*iz+m[13],oz=m[2]*ix+m[6]*iy+m[10]*iz+m[14],ow=m[3]*ix+m[7]*iy+m[11]*iz+m[15];out[0]=(ox/ow+1)/2;out[1]=(oy/ow+1)/2;out[2]=(oz/ow+1)/2;if(out.length>3){out[3]=ow};return out}
//TOVIEWPORT=(out,p)=>{var V=VP_DATA;out[0]=REMAP(p[0],0,1,V[0],V[0]+V[2]);out[1]=REMAP(p[1],1,0,0,V[1]+V[3]);return out}
//INVERTMAT4=(v)=>(new DOMMatrix(v)).inverse().toFloat32Array();
//MULTMAT4=(t,n,r)=>{t.set((new DOMMatrix(n)).multiply(new DOMMatrix(r)).toFloat32Array());return t} //SLOOOOW!!!!!!!!!

//WebGL stuff ***********
SHADER=(src,t=0)=>{
    var sh = gl.createShader(35632+t);
    gl.shaderSource(sh,"#version 300 es\nprecision highp float;\n#define IN uniform\n#define S2D sampler2D\n#define NORM normalize\nin vec3 pos;\n"+(t?"":"out vec4 fragColor;\n")+src);
    gl.compileShader(sh);
    //* THIS CAN BE DELETED 
    if (!gl.getShaderParameter(sh, gl.COMPILE_STATUS)) {
      const info = gl.getShaderInfoLog(sh);
      throw `Could not compile WebGL program. \n\n${info}`;
    }
    //*/
    return sh;
  }

_vs="in vec2 uv;out vec2 _uv;IN float psize;IN mat4 viewp;IN mat4 model;void main(){_uv=uv;gl_Position = viewp*model*vec4(pos,1.0);gl_PointSize=psize;;}"
_fs="IN vec4 color;void main(){fragColor=color;;}"
_texfs="IN S2D tex;IN vec4 color;void main(){fragColor=color*texture(tex,_uv);;}"
quad_vs="out vec2 _uv;void main(){ _uv=pos.xy*0.5+vec2(0.5);gl_Position=vec4(pos,1.0);;}"
UNIFUNCS={5124:"1i",5125:"1ui",5126:"1f",35664:"2fv",35665:"3fv",35666:"4fv",35676:"Matrix4fv",35678:"1i"}

PROGRAM=(vs=_vs,fs=_fs,extra=[])=>{
    var str=extra.map(a=>"#define "+a).join("\n"),i,num,p=gl.createProgram(),getp=gl.getProgramParameter.bind(gl)
    gl.attachShader(p,SHADER(str+vs,1));gl.attachShader(p,SHADER(str+fs));gl.linkProgram(p)
    /* THIS CAN BE DELETED 
    if (!getp(p,35714)) {
        const info = gl.getProgramInfoLog(p);
        throw `Could not compile WebGL program. \n\n${info}`;
      }    
    //*/
    //extract all uniforms
    p.unif={},p.attr={}
    for(i=0,num=getp(p,35718);i<num;++i)
    {
        var loc=gl.getActiveUniform(p,i),n=loc.name
        n=n.substr(0,n.length-(loc.size>1?3:0));
        (p.unif[n]=loc).loc=gl.getUniformLocation(p,n)
        loc.func=gl["uniform"+UNIFUNCS[loc.type]].bind(gl)
        loc.set=loc.type==35676?function(v){this.func(this.loc,false,v)}:function(v){this.func(this.loc,v)}
    }
    for(i=0,num=getp(p,35721);i<num;++i)
    {
        var loc=gl.getActiveAttrib(p,i);
        (p.attr[loc.name]=loc).loc=i
    }
    p.bind=()=>gl.useProgram(p)
    p.uniforms=(u)=>{p.bind();if(u)for(var i in u)p.unif[i]?.set(u[i]);return p}
    p.set=(n,v)=>p.unif[n]?.set(v)
    return p
}

//const BUFFCOMPS={n:1,uv:2,pos:3,rgba:4,offs:4}
//set num=0 if ELEMENT_ARRAY_BUFFER
//s: static_draw, dyn:35048
BUFFER=(d,num=3,s=35044)=>{
    var b=gl.createBuffer(),t=34962+(!num?1:0)//gl.ELEMENT_ARRAY_BUFFER:gl.ARRAY_BUFFER;
    if(d.constructor==Array)d=!num?new Uint16Array(d):V3(d);
    b.b=()=>gl.bindBuffer(t,b)
    b.update=(v)=>(v?b.d.set(v):0,b.b(),gl.bufferData(t,b.d,s),b)
    b.bind=(loc,is_inst=0)=>{
        b.b()
        if(loc==null)return
        gl.vertexAttribPointer(loc,num,5126,false,0,0)//gl.FLOAT
        gl.enableVertexAttribArray(loc)
        gl.vertexAttribDivisor(loc,is_inst)
    }
    b.map=function(f){this.update(this.d.map(f))}
    b.size=d.length;b.num=num;b.d=d
    b.b()
    gl.bufferData(t,b.d,s)//upload
    return b
}

//the attrib name defines the num of components based on name length: pos:3, uv:2, rgba:4, ...
MESH=(d)=>{
    var m={};for(var i in d)if(d[i])m[i]=BUFFER(d[i],i=="tris"?0:i.length)
    m[0]=m.tris?m.tris.size:m.pos.size/3 //total num of primitives
    return m
}

//if hd=10 then is HALF_FLOAT
TEX=(w,h,d=null,depth=0)=>{
    var t=gl.createTexture(),t2=3553,i//gl.TEXTURE_2D
    t.w=w|0;t.h=h|0;
    t.bind=(s=0)=>(gl.activeTexture(33984+s),gl.bindTexture(3553,t),t)
    t.param=(n,v)=>(t.bind(),gl.texParameteri(t2,10240+n,v),t)
    t.toVP=(sh,uni)=>{t.bind();DRAW(QUAD,sh||QUADSHADER,uni||{tex:0})};
    t.mips=()=>{t.bind();gl.generateMipmap(3553)}
    (t.update=(d2)=>(t.bind(),gl.texImage2D(t2,0,depth?33190:6408,w|0,h|0,0,depth?6402:6408,depth?5125:5121,d2)))(d);
    for(i=0;i<4;++i)t.param(i,[9729,33071][(i/2)|0]) //sets basic texture modes
    //t.param(0,9729);//mag 9728:NEAREST 9729:LINEAR
    //t.param(1,9729);//min 9987:LINEAR_MIPMAP_LINEAR 
    //t.param(2,33071);//wraps 33071:clamp_to_edge 10497:gl.REPEAT
    //t.param(3,33071);//wrapt
    return t;
}

FBO=(tex,depth)=>{
    var f=gl.createFramebuffer();
    f.tex=tex
    var w=f.w=tex.w,h=f.h=tex.h,t=36009//gl.DRAW_FRAMEBUFFER;
    gl.bindFramebuffer(36160,f);
    gl.framebufferTexture2D(t,36064,3553,tex,0)//gl.COLOR_ATTACHMENT0
    f.depth=depth||TEX(w,h,null,1)
    gl.framebufferTexture2D(t,36096,3553,f.depth,0);//gl.DEPTH_ATTACHMENT
    /* //should run faster if no depth texture is passed but adds more lines
        depth=gl.createRenderbuffer()
        gl.bindRenderbuffer(36161,depth)
        gl.renderbufferStorage(36161,33190,w,h)//gl.DEPTH_COMPONENT24
        gl.framebufferRenderbuffer(t,36096,36161,depth)//gl.DEPTH_ATTACHMENT
    */
    /* Enable to debug
    var complete = gl.checkFramebufferStatus(t);
	if(complete !== gl.FRAMEBUFFER_COMPLETE) //36054: GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
		throw("FBO not complete: " + complete);
    //*/
    f.bind=(v)=>(gl.bindFramebuffer(36160,v?f:null),VIEWPORT(0,0,v?w:C.width,v?h:C.height),f)
    f.bind(0)
    return f;
}

GLSET=(t,v=1)=>v?gl.enable(t):gl.disable(t)
CLEAR=(c,d)=>{if(c)gl.clearColor(c[0],c[1],c[2],c[3]);gl.clear((c?16384:0)|(d?256:0))}
CULL=2884,ZTEST=2929,BLEND=3042,NEAREST=9728,LINEAR=9729
blendf={A:[770,771],B:[770,1]}
BLENDFUNC=(v)=>{var f=blendf[v];gl.blendFunc(f[0],f[1]);GLSET(BLEND,1)}
DEPTHW=(v=1)=>gl.depthMask(v)


PLANE=(s=1)=>MESH({pos:[-s,0,-s,s,0,s,s,0,-s,s,0,s,-s,0,-s,-s,0,s],uv:[0,0,1,1,1,0, 1,1,0,0,0,1]})
PLANEG={pos:[-1,-1,0, 1,-1,0, 1,1,0, -1,1,0],tris:[0,1,2,0,2,3]}
//CUBE=(s=1,y=0)=>MESH({pos:[-s,y-s,-s,-s,y-s,s,-s,s+y,-s,-s,s+y,s,s,y-s,-s,s,y-s,s,s,s+y,-s,s,s+y,s],tris:"051045237276567546013032173157024264".split("").map(a=>a|0)})

QUAD=0,QUADSHADER=0,FLATSHADER=0,meshes={}

//insts is an obj with BUFFERS that will be fetch per instance
DRAW=(mesh,sh,uni,t=4,num=1,insts=0)=>{
    if(mesh&&mesh.length)mesh=meshes[mesh]//string
    if(!mesh||!mesh[0])return;
    sh=sh||FLATSHADER
    sh.bind()
    sh.unif.viewp?.set(VP_M4)
    sh.unif.ceye?.set(CAM_EYE)
    sh.unif.cfront?.set(CAM_FRONT)
    sh.uniforms(uni)
    for(var i in mesh) if(mesh[i].bind) mesh[i].bind(sh.attr[i]?.loc)
    if(insts)for(var i in insts) insts[i].bind(sh.attr[i].loc,1)
    if(mesh.tris) gl.drawElementsInstanced(t,mesh[0],5123,0,num)//Uint16
    else gl.drawArraysInstanced(t,0,mesh[0],num)
    /* old version that didnt abuse instancing
    if(num_inst){
        for(var i in insts)
            insts[i].bind(sh.attr[i].loc)
        if(mesh.tris) gl.drawElementsInstanced(t,mesh[0],5123,0,num_inst)//Uint16
        else gl.drawArraysInstanced(t,0,mesh[0],num_inst)
    }
    else{
        if(mesh.tris) gl.drawElements(t,mesh[0],5123,0)//Uint16
        else gl.drawArrays(t,0,mesh[0])
    }
    */
}


function _INITGL(){
gl.ext={};gl.getSupportedExtensions().forEach(a=>gl.ext[a]=gl.getExtension(a))
QUAD=MESH(PLANEG);
QUADSHADER=PROGRAM(quad_vs,"in vec2 _uv;IN S2D tex;void main(){fragColor=texture(tex,_uv);;}");
FLATSHADER=PROGRAM(_vs,_fs).uniforms({psize:1});
}

//allows to do basic frustrum culling using spheres
EXTRACTPLANES=(VP)=>{
    FPLANES=[];
    for(var i=0;i<6;++i)
    {
        var s=(i%2?1:-1),j=i>>1,P=V3([VP[3]+s*VP[j],VP[7]+s*VP[4+j],VP[11]+s*VP[8+j],VP[15]+s*VP[12+j]]),L=LEN(P)
        if(!L)L=0.0001//continue;
        for(var j=0;j<4;++j)P[j]/=L;
        FPLANES.push(P)
    }
}

//with spheres
//DISTTOPLANE=(PL,P)=>DOT(PL,P)+PL[3]
//CAMTESTSPHERE=(c,r)=>{for(var i=0;i<6;++i) if(DISTTOPLANE(FPLANES[i],c)<-r)return 0;return 1}

//with AABB
PLANEOVERLAP=(p,c,hs)=>{
	var r=ABS(hs[0]*p[0])+ABS(hs[1]*p[1])+ABS(hs[2]*p[2]),d=DOT(p,c)+p[3]
	if (d<=-r)return 0;
	return (d<=r)?1:2//OVERLAPS:IS_INSIDE
}
CAMTESTBOX=(c,hs)=>{
	var o=0
    for(var i=0,f;i<6;++i){
        if(!(f=PLANEOVERLAP(FPLANES[i],c,hs)))return 0;
        o+=f==2?1:0;
    }
	return o?1:2//OVERLAPS:IS_INSIDE
}

/* useful functions that you may want to use


//allows to extrude a set of 2D points vertically
EXTRUDE=(l,h)=>{
    var v=[],s=l.length
    for(var i=0;i<s;i+=2)
        v.push(l[i],h,l[i+1],l[i],0,l[i+1], l[(i+2)%s],h,l[(i+3)%s],l[(i+2)%s],0,l[(i+3)%s])
    return MESH({pos:new F32(v)})
}

//allows to create a mesh from a line by revoling from vertical axis
LAZO=(l,s=24,m=0,c=0)=>{
    var pos=[],tris=[],rgba=c?[]:0,a=2*PI/s,l1=l.length,t=(m||s),l3=l1/3
    for(var j=0;j<t;j++){
        for(i=0;i<l1;i+=3)
        {
            pos.p(...ROTY(0,[l[i],l[i+1],l[i+2]],a*j))
            if(i<l1-3)tris.p(i?(j*l3+i/3):0,i?(j+1)*l3+i/3:0,j*l3+i/3+1, j*l3+i/3+1,i?(j+1)*l3+i/3:0,(j+1)*l3+(i/3)+1)
        }
        if(c)rgba=rgba.c(c)//for colors
    }
    return MESH({pos,rgba,tris:tris.m(a=>a%(l1*t/3))})
}

//merges two meshes
MERGE=(a,b)=>{
    var m={},l=a.pos.size/3
    for(var i in a)if(i!=0)m[i]=Array.from(a[i].d).concat(Array.from(b[i].d).map(v=>i=="tris"?v+l:v))
    return MESH(m)
}

//subdivides mesh, useful for floor planes or spheres
SUBD=(m,n)=>{
    var tris=[],p=m.pos,t=m.tris,l=p.length/3;
    var mid=(a,b)=>{a*=3;b*=3;var r=LERPV3(V3(),p.slice(a,a+3),p.slice(b,b+3),.5);if(n)NORM(r,r);return p.p(r[0],r[1],r[2])/3-1}
    for(var i=0;i<t.length;i+=3){var a=t[i],b=t[i+1],c=t[i+2],ab=mid(a,b),bc=mid(b,c),ca=mid(c,a);tris.p(a,ab,ca,ab,bc,ca,ab,b,bc,bc,c,ca)}
    return {pos:p,tris}
}
b={pos:[1,0,0,0,1,0,0,0,1],tris:[0,1,2]}
for(i=0;i<3;++i)b=SUBD(b,1)
SIM(b.pos,b.tris,[-1,1,1]);SIM(b.pos,b.tris,[1,1,-1])
meshes.hemi=MESH(b);SIM(b.pos,b.tris,[1,-1,1]);meshes.ball=MESH(b)

b={pos:PLANEG.pos,tris:PLANEG.tris}
for(i=0;i<6;++i)b=SUBD(b)
meshes.grid=MESH(b)

*/

