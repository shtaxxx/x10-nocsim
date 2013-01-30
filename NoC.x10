package noc;
import x10.util.Random;
import x10.util.ArrayList;
import x10.util.List;
import x10.lang.Math;
import x10.lang.System;

/******************************************************************************/
interface NoCBase
{
  static val INJECTION_RATE = 0.1;
  static val CHIP_SIZE = 4096;
  static val SIZE_X = 64;
  static val SIZE_Y = 64;
  //static val CHIP_SIZE = 1024;
  //static val SIZE_X = 32;
  //static val SIZE_Y = 32;
  //static val CHIP_SIZE = 256;
  //static val SIZE_X = 16;
  //static val SIZE_Y = 16;
  //static val CHIP_SIZE = 16;
  //static val SIZE_X = 4;
  //static val SIZE_Y = 4;
  static val NUM_CH = 2;
  static val BUF_SIZE = 4;
  static val PACKET_LEN = 8;
  static val SEND_CH = 0;

  static val FT_HEAD = 0;
  static val FT_BODY = 1;
  static val FT_TAIL = 2;
  static val NUM_DIR = 5;
  static val DN = 0;
  static val DE = 1;
  static val DS = 2;
  static val DW = 3;
  static val DC = 4;
}

/******************************************************************************/
class Flit implements NoCBase
{
  var valid : Boolean;
  var flittype : Int;
  var ch : Int;
  var src : Point(2);
  var dst : Point(2);
  var payload : Int;
  var timestamp : Int;

  def this()
  {
    this.valid = false;
    this.flittype = 0;
    this.ch = 0;
    this.src = [0, 0];
    this.dst = [0, 0];
    this.payload = 0;
    this.timestamp = 0;
  }

  def this(flittype : Int, ch : Int, src : Point(2), dst : Point(2), payload : Int, timestamp : Int)
  {
    this.valid = true;
    this.flittype = flittype;
    this.ch = ch;
    this.src = src;
    this.dst = dst;
    this.payload = payload;
    this.timestamp = timestamp;
  }
  
  def clone()
  {
    var r : Flit = new Flit(flittype, ch, src, dst, payload, timestamp);
    r.valid = this.valid;
    return r;
  }
}

class FlowControl implements NoCBase
{
  var value : Array[Boolean](1);
  def this()
  {
    this.value = new Array[Boolean](NUM_CH, false);
  }
  def set(new_value : Array[Boolean](1))
  {
    for([n] in new_value){
      this.value(n) = new_value(n);
    }
  }
  def get()
  {
    return this.value;
  }
  def clone()
  {
    var r : FlowControl = new FlowControl();
    for([i] in r.value){
      r.value(i) = this.value(i);
    }
    return r;
  }

}

/******************************************************************************/
class Router implements NoCBase
{
  val id : Point(2);
  
  var input : Array[Flit](1);
  var output : Array[Flit](1);
  var infcontrol : Array[FlowControl](1);
  var outfcontrol : Array[FlowControl](1);
  
  var input_buffers : Array[ArrayList[Flit]](1);
  
  //Input side
  var ib_head : Array[Boolean](1);
  var ib_nextdir : Array[Int](1);
  var reg_ib_head : Array[Boolean](1);
  var reg_ib_nextdir : Array[Int](1);
  
  var vc_locked_in : Array[Boolean](1);
  var reg_vc_locked_in : Array[Boolean](1);

  //Output side
  var vc_locked : Array[Boolean](1);
  var vc_allocated: Array[Int](1);
  var reg_vc_locked : Array[Boolean](1);
  var reg_vc_allocated : Array[Int](1);
  
  //Output side
  var sw_locked : Array[Boolean](1);
  var sw_allocated : Array[Int](1);
  var sw_selected_outvc : Array[Int](1);
  var reg_sw_locked : Array[Boolean](1);
  var reg_sw_allocated : Array[Int](1);
  var reg_sw_selected_outvc : Array[Int](1);

  var credit : Array[Int](1);
  var reg_credit : Array[Int](1);

  var st_output_valid : Array[Boolean](1);
  var st_output : Array[Flit](1);
  var st_vc_release : Array[Int](1);
  var reg_st_output_valid : Array[Boolean](1);
  var reg_st_output : Array[Flit](1);
  //var reg_st_vc_release : Array[Int](1);

  var lt_output_valid : Array[Boolean](1);
  var lt_output : Array[Flit](1);
  var reg_lt_output_valid : Array[Boolean](1);
  var reg_lt_output : Array[Flit](1);
  
  //for Input buffer
  var deque_candidate : Array[Boolean](1);
  
  def this(id : Point(2))
  {
    this.id = id;
    //Console.OUT.println("My ID is " + this.id);
    this.input = new Array[Flit](NUM_DIR);
    this.output = new Array[Flit](NUM_DIR);
    this.infcontrol = new Array[FlowControl](NUM_DIR);
    this.outfcontrol = new Array[FlowControl](NUM_DIR);
    this.input_buffers = new Array[ArrayList[Flit]](NUM_DIR * NUM_CH);

    for([i] in input){
      input(i) = new Flit();
    }
    for([i] in output){
      output(i) = new Flit();
    }
    for([i] in infcontrol){
      infcontrol(i) = new FlowControl();
    }
    for([i] in outfcontrol){
      outfcontrol(i) = new FlowControl();
    }
    for([i] in input_buffers){
      input_buffers(i) = new ArrayList[Flit]();
    }
    
    this.ib_head = new Array[Boolean](NUM_DIR * NUM_CH, false);
    this.ib_nextdir = new Array[Int](NUM_DIR * NUM_CH, -1);
    this.reg_ib_head = new Array[Boolean](NUM_DIR * NUM_CH, false);
    this.reg_ib_nextdir = new Array[Int](NUM_DIR * NUM_CH, -1);
    
    this.vc_locked_in = new Array[Boolean](NUM_DIR * NUM_CH, false);
    this.reg_vc_locked_in = new Array[Boolean](NUM_DIR * NUM_CH, false);

    this.vc_locked = new Array[Boolean](NUM_DIR * NUM_CH, false);
    this.vc_allocated = new Array[Int](NUM_DIR * NUM_CH, -1);
    this.reg_vc_locked = new Array[Boolean](NUM_DIR * NUM_CH, false);
    this.reg_vc_allocated = new Array[Int](NUM_DIR * NUM_CH, -1);
    
    this.sw_locked = new Array[Boolean](NUM_DIR, false);
    this.sw_allocated = new Array[Int](NUM_DIR, -1);
    this.sw_selected_outvc = new Array[Int](NUM_DIR, 0);
    this.reg_sw_locked = new Array[Boolean](NUM_DIR, false);
    this.reg_sw_allocated = new Array[Int](NUM_DIR, -1);
    this.reg_sw_selected_outvc = new Array[Int](NUM_DIR, 0);

    this.st_output_valid = new Array[Boolean](NUM_DIR, false);
    this.st_output = new Array[Flit](NUM_DIR);
    this.st_vc_release = new Array[Int](NUM_DIR, -1);
    this.reg_st_output_valid = new Array[Boolean](NUM_DIR, false);
    this.reg_st_output = new Array[Flit](NUM_DIR);
    //this.reg_st_vc_release = new Array[Int](NUM_DIR, -1);

    for([i] in st_output){
      st_output(i) = new Flit();
    }
    for([i] in reg_st_output){
      reg_st_output(i) = new Flit();
    }

    this.lt_output_valid = new Array[Boolean](NUM_DIR, false);
    this.lt_output = new Array[Flit](NUM_DIR);
    this.reg_lt_output_valid = new Array[Boolean](NUM_DIR, false);
    this.reg_lt_output = new Array[Flit](NUM_DIR);

    for([i] in lt_output){
      lt_output(i) = new Flit();
    }
    for([i] in reg_lt_output){
      reg_lt_output(i) = new Flit();
    }

    this.credit = new Array[Int](NUM_DIR * NUM_CH, BUF_SIZE);
    this.reg_credit = new Array[Int](NUM_DIR * NUM_CH, BUF_SIZE);
    
    this.deque_candidate = new Array[Boolean](NUM_DIR * NUM_CH, false);
    
  }
  
  def routeComputation([dx, dy] : Point(2))
  {
    //Console.OUT.println("ID:" + id + " dst:" + [dx, dy]);
    //Console.OUT.println("toX+");
    if(dx > id(0)) return DE;
    //Console.OUT.println("toX-");
    if(dx < id(0)) return DW;
    //Console.OUT.println("toY+");
    if(dy > id(1)) return DS;
    //Console.OUT.println("toY-");
    if(dy < id(1)) return DN;
    //Console.OUT.println("toC");
    return DC;
  }
  
  def step()
  {
    //Console.OUT.println("ID:" + id);
    drive();
    update();
  }
  
  def drive()
  {
    //Console.OUT.println("drive");
    drive_credit();
    //Console.OUT.println("drive credit");
    drive_inputBuffer();
    //Console.OUT.println("drive IB");
    drive_routeComputation();
    //Console.OUT.println("drive RC");
    drive_virtualChannelAllocation();
    //Console.OUT.println("drive VA");
    drive_switchAllocation();
    //Console.OUT.println("drive SA");
    drive_switchTraversal();
    //Console.OUT.println("drive ST");
    drive_linkTraversal();
    //Console.OUT.println("drive LT");
    //Console.OUT.println("drive done");
  }

  def update()
  {
    //Console.OUT.println("update");
    update_credit();
    //Console.OUT.println("update credit");
    update_inputBuffer();
    //Console.OUT.println("update IB");
    update_routeComputation();
    //Console.OUT.println("update RC");
    update_virtualChannelAllocation();
    //Console.OUT.println("update VA");
    update_switchAllocation();
    //Console.OUT.println("update SA");
    update_switchTraversal();
    //Console.OUT.println("update ST");
    update_linkTraversal();
    //Console.OUT.println("update LT");
    //Console.OUT.println("update done");
  }
  
  def drive_credit()
  {
    var pos : Int = 0;
    for([inf] in infcontrol){
      credit(pos) = reg_credit(pos);
      val value : Array[Boolean](1) = infcontrol(inf).get();
      for([v] in value){
        if(value(v)){
          credit(pos) = credit(pos) + 1;
          //Console.OUT.println("ID:"+id + "credit inc:" + pos);
        }
        pos++;
      }
    }
  }

  def update_credit()
  {
    var pos : Int = 0;
    for([r] in reg_credit){
      reg_credit(pos) = credit(pos);
      pos ++;
    }
  }
  
  //IB stage
  def drive_inputBuffer()
  {
    for([indir] in input){
      val flit = input(indir);
      val ch = flit.ch;
      val valid = flit.valid;
      if(valid){
        input_buffers(indir*NUM_CH+ch).add(flit);
        //Console.OUT.println("Input flit:" + id + ":" + (indir*NUM_CH+ch));
      }
    }
    for([dc] in deque_candidate){
      deque_candidate(dc) = false;
    }
  }

  def update_inputBuffer()
  {
    for([dc] in deque_candidate){
      if(deque_candidate(dc)){
        input_buffers(dc).removeFirst();
        //Console.OUT.println("Deque flit: " + id + ":" + dc);
      }
    } 
  }
  
  //RC stage
  def drive_routeComputation()
  {
    for([ib] in input_buffers){
      ib_head(ib) = false;
      ib_nextdir(ib) = -1;
      if(reg_vc_locked_in(ib)) continue;
      if(input_buffers(ib).isEmpty()) continue;
      val flit = input_buffers(ib).getFirst();
      if(flit.flittype == FT_HEAD){ 
        ib_nextdir(ib) = routeComputation(flit.dst);
        //Console.OUT.println("ID:" + id + " ib_nextdir(ib)=" + ib_nextdir(ib));
        ib_head(ib) = true;
      }
    }
  }
  
  def update_routeComputation()
  {
    for([i] in ib_nextdir){
      reg_ib_nextdir(i) = ib_nextdir(i);
    }
    for([i] in ib_head){
      reg_ib_head(i) = ib_head(i);
    }
  }
  
  //VC stage
  def gather_my_request(mydir : Int, req : Array[Int](1))
  {
    var my_req : Array[Int](1) = new Array[Int](NUM_DIR*NUM_CH, -1);
    var pos : Int = 0;
    var cnt : Int = 0;
    for([r] in req){
      if(req(r) == mydir){
        my_req(cnt) = pos;
        cnt += 1;
      }
      pos += 1;
    }
    return my_req;
  }
  
  def select_one_request(last_pos : Int, my_req : Array[Int](1))
  {
    var start_pos : Int = last_pos + 1;
    if(start_pos >= NUM_DIR * NUM_CH) start_pos = 0;
    for([i] in my_req){
      if(my_req(i) < 0) break;  
      if(my_req(i) >= start_pos) {
        return my_req(i);
      }
    }
    for([i] in my_req){
      if(my_req(i) < 0) break;
      return my_req(i);
    }
    return -1;
  }
  
  def select_one_grant(in_dir : Int, in_ch : Int, req_dir : Int, vc_grant : Array[Int](1))
  {
    if(req_dir < 0) return -1;
    for(var p : Int = req_dir * NUM_CH; p < (req_dir+1)*NUM_CH; p++){
      if(vc_grant(p) == in_dir*NUM_CH+in_ch) { //has grant
        return p;
      }
    }
    return -1;
  }
  
  def drive_virtualChannelAllocation()
  {
    for([a] in reg_vc_locked_in){
      vc_locked_in(a) = reg_vc_locked_in(a);
    }
    for([a] in reg_vc_allocated){
      vc_locked(a) = reg_vc_locked(a);
      vc_allocated(a) = reg_vc_allocated(a);
    }

    //iSlip arbitration
    //Request -> Grant
    var vc_grant : Array[Int](1) = new Array[Int](NUM_DIR * NUM_CH, -1);
    for(var out_dir : Int = 0; out_dir < NUM_DIR; out_dir++){
      for(var out_ch : Int = 0; out_ch < NUM_CH; out_ch++){
        if(reg_vc_locked(out_dir*NUM_CH + out_ch)) continue;
        if(reg_credit(out_dir*NUM_CH + out_ch) < BUF_SIZE) continue;
        var my_req : Array[Int](1) = gather_my_request(out_dir, reg_ib_nextdir);
        //Console.OUT.println("ID:" + id + " reg_ib_nextdir:" + reg_ib_nextdir);
        //Console.OUT.println("my_req:" + my_req);
        var last_ch : Int = reg_vc_allocated(out_dir*NUM_CH+out_ch);
        var selected_ch : Int = select_one_request(last_ch, my_req);
        if(selected_ch >= 0) {
          vc_grant(out_dir*NUM_CH + out_ch) = selected_ch;
        }
      }
    }

    //Grant -> Ack
    var vc_ack : Array[Int](1) = new Array[Int](NUM_DIR * NUM_CH, -1);
    for(var in_dir : Int = 0; in_dir < NUM_DIR; in_dir++){
      for(var in_ch : Int = 0; in_ch < NUM_CH; in_ch++){
        val req_dir = reg_ib_nextdir(in_dir*NUM_CH+in_ch);
        vc_ack(in_dir*NUM_CH+in_ch) = select_one_grant(in_dir,in_ch,req_dir,vc_grant);
      }
    }
    
    //Ack -> to Register
    for(var out_dir : Int = 0; out_dir < NUM_DIR; out_dir++){
      for(var out_ch : Int = 0; out_ch < NUM_CH; out_ch++){
        if(vc_grant(out_dir*NUM_CH+out_ch) < 0) break;
        val cand_vc : Int = vc_grant(out_dir*NUM_CH+out_ch);
        if(vc_ack(cand_vc) == out_dir*NUM_CH+out_ch){
          vc_locked(out_dir*NUM_CH+out_ch) = true;
          vc_allocated(out_dir*NUM_CH+out_ch) = cand_vc;
          vc_locked_in(cand_vc) = true;
          //Console.OUT.println("ID:" + id + " allocated: " + cand_vc + " to " + (out_dir*NUM_CH+out_ch) );
        }
      }
    }
  }
  
  def update_virtualChannelAllocation()
  {
    for([a] in vc_allocated){
      reg_vc_locked_in(a) = vc_locked_in(a);
      reg_vc_locked(a) = vc_locked(a);
      reg_vc_allocated(a) = vc_allocated(a);
    }
    var dir : Int = 0;
    for([s] in st_vc_release){
      if(st_vc_release(s) >= 0){
        val release_ch = dir*NUM_CH+st_vc_release(s);
        val input_release_ch = reg_vc_allocated(release_ch);
        reg_vc_locked_in(input_release_ch) = false;
        reg_vc_allocated(release_ch) = -1;
        reg_vc_locked(release_ch) =  false;
        //Console.OUT.println("ID:" + id + " released: " + release_ch);
      }
      dir += 1;
    }
  }
  
  //SA stage
  def drive_switchAllocation()
  {
    for(var out_dir : Int = 0; out_dir < NUM_DIR; out_dir++){
      sw_allocated(out_dir) = -1;
      sw_locked(out_dir) = false;

      var selected_ch : Int = -1;
      var start_ch : Int = reg_sw_allocated(out_dir) + 1;
      if(start_ch >= NUM_CH) start_ch = 0;
      for(var p:Int = start_ch; p < NUM_CH; p++){
        if(! reg_vc_locked(out_dir*NUM_CH+p)) continue;
        val in_ch = reg_vc_allocated(out_dir*NUM_CH+p);
        if(input_buffers(in_ch).size() == 0) continue;
        if(reg_sw_locked(out_dir) &&
           reg_sw_allocated(out_dir) == in_ch &&
           input_buffers(in_ch).size() <= 1) continue;
        if(reg_credit(out_dir*NUM_CH+p) == 0) continue;
        selected_ch = p;
        break;
      }

      for(var p:Int = 0; p < NUM_CH; p++){
        if(selected_ch != -1) break;
        if(! reg_vc_locked(out_dir*NUM_CH+p)) continue;
        val in_ch = reg_vc_allocated(out_dir*NUM_CH+p);
        if(input_buffers(in_ch).size() == 0) continue;
        if(reg_sw_locked(out_dir) &&
           reg_sw_allocated(out_dir) == in_ch &&
           input_buffers(in_ch).size() <= 1) continue;
        if(reg_credit(out_dir*NUM_CH+p) == 0) continue;
        selected_ch = p;
        break;
      }
      
      if(selected_ch >= 0){
        sw_locked(out_dir) = true;
        sw_allocated(out_dir) = reg_vc_allocated(out_dir*NUM_CH+selected_ch);
        sw_selected_outvc(out_dir) = selected_ch;
      }
    }
  }

  def update_switchAllocation()
  {
    for([l] in sw_locked){
      reg_sw_locked(l) = sw_locked(l);
    }
    for([a] in sw_allocated){
      reg_sw_allocated(a) = sw_allocated(a);
    }
    for([a] in sw_allocated){
      reg_sw_selected_outvc(a) = sw_selected_outvc(a);
    }
  }
  
  //ST stage
  def drive_switchTraversal()
  {
    for([l] in reg_sw_locked){
      st_output_valid(l) = false;
      st_vc_release(l) = -1;
      if(reg_sw_locked(l)){
        val src_ch : Int = reg_sw_allocated(l);
        st_output(l) = input_buffers(src_ch).getFirst();
        st_output_valid(l) = true;
        st_output(l).ch = reg_sw_selected_outvc(l);
        if(st_output(l).flittype == FT_TAIL){
          st_vc_release(l) = reg_sw_selected_outvc(l);
        }
        deque_candidate(src_ch) = true;
        val dec_pos = l*NUM_CH+reg_sw_selected_outvc(l);
        credit(dec_pos) -= 1;
        //Console.OUT.println("ID:"+id + "credit dec:" + dec_pos);
      }
    }
  }
  
  def update_switchTraversal()
  {
    for([v] in st_output_valid){
      reg_st_output_valid(v) = st_output_valid(v);
    }
    for([s] in st_output){
      reg_st_output(s) = st_output(s);
    }

    for(var dir : Int = 0; dir < NUM_DIR; dir++){
      var new_outfcontrol_value : Array[Boolean](1) = new Array[Boolean](NUM_CH);
      for(var ch : Int = 0; ch < NUM_CH; ch++){
        new_outfcontrol_value(ch) = deque_candidate(dir*NUM_CH+ch);  
      }
      outfcontrol(dir).set(new_outfcontrol_value);  
    }
  }
  
  //LT stage
  def drive_linkTraversal()
  {
    for([v] in reg_st_output_valid){
      lt_output_valid(v) = reg_st_output_valid(v);
    }
    for([v] in reg_st_output){
      lt_output(v) = reg_st_output(v);
    }
  }

  def update_linkTraversal()
  {
    for([v] in lt_output_valid){
      reg_lt_output_valid(v) = lt_output_valid(v);
    }
    for([s] in lt_output){
      reg_lt_output(s) = lt_output(s);
      if(lt_output_valid(s)){
        output(s) = lt_output(s);
        //Console.OUT.println("LT ID:" + id + " DIR:" + s + "valid:" + output(s).valid  + " src:" + output(s).src + " dst:" + output(s).dst + " type:" + output(s).flittype);
      }else{
        output(s) = new Flit();
      }
    }
  }  
}


/******************************************************************************/
class Core implements NoCBase
{
  val id : Point(2);
  var input : Flit;
  var output : Flit;
  var infcontrol : FlowControl;
  var outfcontrol : FlowControl;

  var credit : Array[Int](1);
  var reg_credit : Array[Int](1);

  var send_flit_queue : ArrayList[Flit];

  var send_wait : Int;
  var injc_flit_cnt : Int;
  var send_flit_cnt : Int;
  var recv_flit_cnt : Int;
  var recv_time_sum : Int;
  var random : Random;
  var timecnt : Int;
  
  def this(id : Point(2))
  {
    this.id = id;
    //Console.OUT.println("My ID is " + this.id);

    this.input = new Flit();
    this.output = new Flit();
    this.infcontrol = new FlowControl();
    this.outfcontrol = new FlowControl();
    this.credit = new Array[Int](NUM_CH, BUF_SIZE);
    this.reg_credit = new Array[Int](NUM_CH, BUF_SIZE);

    this.send_wait = 0;
    this.injc_flit_cnt = 0;
    this.send_flit_cnt = 0;
    this.recv_flit_cnt = 0;
    this.recv_time_sum = 0;

    this.send_flit_queue = new ArrayList[Flit]();
    this.random = new Random();
    this.random.setSeed(System.nanoTime());

    this.timecnt = 0;
  }

  def step()
  {
    drive();
    update();
  }

  def drive()
  {
    drive_credit();
    send();
    recv();
  }

  def update()
  {
    update_credit();
    timecnt ++;
  }

  def get_dst()
  {
    //return get_dst_complement();
    return get_dst_uniform();
  }

  def get_dst_complement()
  {
    val x = id(0);
    val y = id(1);
    val dst_x = SIZE_X - x - 1;
    val dst_y = SIZE_Y - y - 1;
    return [dst_x, dst_y];
  }

  def get_dst_uniform()
  {
    val dst_x = random.nextInt(SIZE_X);
    val dst_y = random.nextInt(SIZE_Y);
    return [dst_x, dst_y];
  }

  def drive_credit()
  {
    var pos : Int = 0;
    val value = infcontrol.get();
    for([v] in value){
      credit(pos) = reg_credit(pos);
      if(value(v)){
        credit(pos) = credit(pos) + 1;
      }
      pos++;
    }
    var new_value : Array[Boolean](1) = new Array[Boolean](NUM_CH, false);
    if(input.valid){
      new_value(input.ch) = true;
    }
    outfcontrol.set(new_value);
  }

  def update_credit()
  {
    var pos : Int = 0;
    if(output.valid){
      credit(SEND_CH) = credit(SEND_CH) - 1;
    }
    for(r in reg_credit){
      reg_credit(pos) = credit(pos);
    }
  }

  def send()
  {
    inject_packet();
    send_flit();
  }

  def recv()
  {
    if(! input.valid) return;
    recv_flit_cnt += 1;
    recv_time_sum += (timecnt - input.timestamp);
    //Console.OUT.println("recv from " + input.src + " to " + id + ". Type: " + input.flittype);
  }

  def inject_packet()
  {
    if(send_wait > 0){
      send_wait--;
      return;
    }
    val randval = random.nextDouble();
    //Console.OUT.println("randval:" + randval);
    if( randval < INJECTION_RATE ){
      val src = id;
      val dst = get_dst();
      val timestamp = timecnt;
      var payload : Int = 0;
      send_flit_queue.add(new Flit(FT_HEAD, SEND_CH, src, dst, payload, timestamp));
      for(var i:Int=0; i<PACKET_LEN-2; i++){
        payload += 1;
        send_flit_queue.add(new Flit(FT_BODY, SEND_CH, src, dst, payload, timestamp));
      }
      payload += 1;
      send_flit_queue.add(new Flit(FT_TAIL, SEND_CH, src, dst, payload, timestamp));
      send_wait = PACKET_LEN * 10;
      injc_flit_cnt += PACKET_LEN;
    }
  }

  def send_flit()
  {
    output = new Flit();
    if(send_flit_queue.isEmpty()) return; //Invalid
    if(reg_credit(SEND_CH) == 0) return; //Invalid
    val f = send_flit_queue.getFirst();
    if(f.flittype == FT_HEAD && reg_credit(SEND_CH) < BUF_SIZE) return; //Invalid
    output = f;
    send_flit_queue.removeFirst();
    send_flit_cnt++;
    //Console.OUT.println("send from " + id + " to " + f.dst + ". Type: " + f.flittype);
  }  
}

/******************************************************************************/
class Chip implements NoCBase
{
  val SIM_CYCLE : Int;

  val routers : DistArray[Router](2);
  val cores : DistArray[Core](2);

  val router_initializer = ([idx,idy]:Point(2)) => {
    new Router([idx, idy])
  };

  val core_initializer = ([idx,idy]:Point(2)) => {
    new Core([idx, idy])
  };
  
  def this(sim_cycle : Int)
  {
    SIM_CYCLE = sim_cycle;
    val R = (0..(SIZE_X-1)) * (0..(SIZE_Y-1));
    val D = Dist.makeBlock(R);
    routers = DistArray.make[Router](D, router_initializer);
    cores = DistArray.make[Core](D, core_initializer);
  }
  
  def step()
  {
    //Console.OUT.println("step");
    val R = (0..(SIZE_X-1)) * (0..(SIZE_Y-1));
    val D = Dist.makeBlock(R);
    val D_Base = Dist.makeUnique(D.places());
    finish ateach(z in D_Base) async{
      for(p:Point(2) in D | here) async{
        cores(p).step();
        routers(p).step();
      }
    }
  }
  
  def communicate()
  {
    //Console.OUT.println("communicate");
    val R = (0..(SIZE_X-1)) * (0..(SIZE_Y-1));
    val D = Dist.makeBlock(R);
    val D_Base = Dist.makeUnique(D.places());
    finish ateach(z in D_Base) async{ 
      for(p:Point(2) in D | here) async{

      //routers(p).refresh_input();

      cores(p).input = routers(p).output(DC);
      cores(p).infcontrol = routers(p).outfcontrol(DC);
      routers(p).input(DC) = cores(p).output;
      routers(p).infcontrol(DC) = cores(p).outfcontrol;
      val x = p(0);
      val y = p(1);
      if(y != 0) {
        routers(p).input(DN) = at(D([x,y-1])) routers([x,y-1]).output(DS);
        routers(p).infcontrol(DN) = at(D([x,y-1])) routers([x,y-1]).outfcontrol(DS);
        //val tmp = at(D([x,y-1])) routers([x,y-1]).output(DS);
        //Console.OUT.println("communicate N " + tmp.dst + ":" + tmp.valid);
      }
      if(x != SIZE_X-1){
        routers(p).input(DE) = at(D([x+1,y])) routers([x+1,y]).output(DW);
        routers(p).infcontrol(DE) = at(D([x+1,y])) routers([x+1,y]).outfcontrol(DW);
        //val tmp = at(D([x+1,y])) routers([x+1,y]).output(DW);
        //Console.OUT.println("communicate E " + tmp.dst + ":" + tmp.valid);
      }
      if(y != SIZE_Y-1){
        routers(p).input(DS) = at(D([x,y+1])) routers([x,y+1]).output(DN);
        routers(p).infcontrol(DS) = at(D([x,y+1])) routers([x,y+1]).outfcontrol(DN);
        //val tmp = at(D([x,y+1])) routers([x,y+1]).output(DN);
        //Console.OUT.println("communicate S " + tmp.dst + ":" + tmp.valid);
      }
      if(x != 0){
        routers(p).input(DW) = at(D([x-1,y])) routers([x-1,y]).output(DE);
        routers(p).infcontrol(DW) = at(D([x-1,y])) routers([x-1,y]).outfcontrol(DE);
        //val tmp = at(D([x-1,y])) routers([x-1,y]).output(DE);
        //Console.OUT.println("communicate W " + tmp.dst + ":" + tmp.valid);
      }
    }}
  }

  def initialize()
  {
    Console.OUT.println("initialize");
  }

  def finilize()
  {
    Console.OUT.println("finalization");
    val R = (0..(SIZE_X-1)) * (0..(SIZE_Y-1));
    val D = Dist.makeBlock(R);
    val D_Base = Dist.makeUnique(D.places());

    val dist_injc_flit_cnt : DistArray[Int](2) = DistArray.make[Int](D, 0);
    val dist_send_flit_cnt : DistArray[Int](2) = DistArray.make[Int](D, 0);
    val dist_recv_flit_cnt : DistArray[Int](2) = DistArray.make[Int](D, 0);
    val dist_recv_time_sum : DistArray[Int](2) = DistArray.make[Int](D, 0);

    finish ateach(z in D_Base) for(p:Point(2) in D | here){
      dist_injc_flit_cnt(p) = cores(p).injc_flit_cnt;
      dist_send_flit_cnt(p) = cores(p).send_flit_cnt;
      dist_recv_flit_cnt(p) = cores(p).recv_flit_cnt;
      dist_recv_time_sum(p) = cores(p).recv_time_sum;
    }
    
    val sum_injc_flit_cnt = dist_injc_flit_cnt.reduce((x:Int,y:Int)=>x+y, 0);
    val sum_send_flit_cnt = dist_send_flit_cnt.reduce((x:Int,y:Int)=>x+y, 0);
    val sum_recv_flit_cnt = dist_recv_flit_cnt.reduce((x:Int,y:Int)=>x+y, 0);
    val sum_recv_time_sum = dist_recv_time_sum.reduce((x:Int,y:Int)=>x+y, 0);

    Console.OUT.println("           Cycles: " + SIM_CYCLE);
    Console.OUT.println("          # Nodes: " + CHIP_SIZE + "(X:" + SIZE_X + ", Y:" + SIZE_Y + ")" );
    Console.OUT.println("   Injection Rate: " + INJECTION_RATE);
    Console.OUT.println("       Send Count: " + sum_send_flit_cnt);
    Console.OUT.println("       Recv Count: " + sum_recv_flit_cnt);
    Console.OUT.println("Actual Injc. Rate: " + ((sum_injc_flit_cnt*1.0)/(SIM_CYCLE*CHIP_SIZE*1.0)));
    Console.OUT.println("    Offerred Rate: " + ((sum_send_flit_cnt*1.0)/(SIM_CYCLE*CHIP_SIZE*1.0)));
    Console.OUT.println("       Throughput: " + ((sum_recv_flit_cnt*1.0)/(SIM_CYCLE*CHIP_SIZE*1.0)));
    Console.OUT.println("     Avg. Latency: " + ((sum_recv_time_sum*1.0)/sum_recv_flit_cnt));
  }  

  def simulation()
  {
    for(i in 0..(SIM_CYCLE-1)){
      step();
      communicate();
    }
  }
}

/******************************************************************************/
public class NoC
{
  //static val SIM_CYCLE = 1000 * 5;
  static val SIM_CYCLE = 500;

  public static def main(args : Array[String]) {
    // TODO auto-generated stub
    val chip = new Chip(SIM_CYCLE);
    chip.initialize();
    chip.simulation();
    chip.finilize();
  }
}
