// See LICENSE for license details.

#ifndef MM_EMULATOR_H
#define MM_EMULATOR_H

#include <stdint.h>
#include <cstring>
#include <queue>

struct mm_rresp_t
{
  uint64_t id;
  std::vector<uint8_t> data;
  bool last;

  mm_rresp_t(uint64_t id, std::vector<uint8_t> data, bool last)
  {
    this->id = id;
    this->data = data;
    this->last = last;
  }

  mm_rresp_t()
  {
    this->id = 0;
    this->last = false;
  }
};

class mm_magic_t
{
 public:
  mm_magic_t(size_t size, size_t word_size);
  ~mm_magic_t();
  void init(size_t sz, int word_size);
  uint8_t* get_data() { return data; }
  size_t get_size() { return size; }

  bool ar_ready() { return true; }
  bool aw_ready() { return !store_inflight; }
  bool w_ready() { return store_inflight; }
  bool b_valid() { return !bresp.empty(); }
  uint64_t b_resp() { return 0; }
  uint64_t b_id() { return b_valid() ? bresp.front() : 0; }
  bool r_valid() { return !rresp.empty(); }
  uint64_t r_resp() { return 0; }
  uint64_t r_id() { return r_valid() ? rresp.front().id: 0; }
  void *r_data() { return r_valid() ? &rresp.front().data[0] : &dummy_data[0]; }
  bool r_last() { return r_valid() ? rresp.front().last : false; }

  void tick
  (
    bool reset,

    bool ar_valid,
    uint64_t ar_addr,
    uint64_t ar_id,
    uint64_t ar_size,
    uint64_t ar_len,

    bool aw_valid,
    uint64_t aw_addr,
    uint64_t aw_id,
    uint64_t aw_size,
    uint64_t aw_len,

    bool w_valid,
    uint64_t w_strb,
    void *w_data,
    bool w_last,

    bool r_ready,
    bool b_ready
  );

  void write(uint64_t addr, uint8_t *data);
  void write(uint64_t addr, uint8_t *data, uint64_t strb, uint64_t size);
  std::vector<uint8_t> read(uint64_t addr);
  void load_mem(const char* fn, const uint64_t base_addr);

 private:
  uint8_t* data;
  size_t size;
  size_t word_size;

  bool store_inflight;
  uint64_t store_addr;
  uint64_t store_id;
  uint64_t store_size;
  uint64_t store_count;
  std::vector<uint8_t> dummy_data;
  std::queue<uint64_t> bresp;

  std::queue<mm_rresp_t> rresp;

  uint64_t cycle;
};

#endif
