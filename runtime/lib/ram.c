////////////////////////////////////////////////////////////////
// Runtime memory support library for ASL's C backend
//
// Copyright Intel Inc (c) 2022
// SPDX-Licence-Identifier: BSD-3-Clause
////////////////////////////////////////////////////////////////

#include "asl/ram.h"

#include <string.h>
#include <stdlib.h>

#include "asl/error.h"

static const int page_entry_sz = 1;
static const int table_entry_sz = sizeof(uint8_t *);

static const int entries_log2 = 16;  // both tables and pages have 64k entries
static const int entries = 1 << entries_log2;

static const int page_sz = page_entry_sz * entries;
static const int level_of_page = 4;

static inline uint64_t
entry_num(uint64_t address)
{
        return address & (entries - 1);
}

static inline uint64_t
page_offset(uint64_t address)
{
        return page_entry_sz * entry_num(address);
}

static inline uint64_t
table_offset(uint64_t address)
{
        return table_entry_sz * entry_num(address);
}

static uint8_t *
page_alloc(uint8_t init_val)
{
        void *p = malloc(page_sz);
        runtime_error_if(!p, "cannot allocate a page");
        memset(p, init_val, page_sz);
        return p;
}

static uint8_t *
table_alloc()
{
        void *p = calloc(entries, table_entry_sz);
        runtime_error_if(!p, "cannot allocate a table");
        return p;
}

static void
table_free(uint8_t *p, int level)
{
        if (!p)
                return;

        if (level < level_of_page) {
                for (int i = 0; i < entries; ++i) {
                        uint8_t *e;
                        memcpy(&e, p + table_entry_sz * i, sizeof(uint8_t *));
                        table_free(e, level + 1);
                }
        }

        free(p);
}

/* When looking for a page, page table structures (tables and pages) are
   allocated on demand. */
static uint8_t *
page_lookup(ASL_ram_t *ram, uint64_t address)
{
        uint8_t *p = ram->p;
        uint8_t **p_addr = &ram->p;

        for (int level = 1; level < level_of_page; ++level) {
                if (!p) {  // no table
                        p = table_alloc();
                        *p_addr = p;
                }

                address >>= entries_log2;
                p_addr = (uint8_t **)(p + table_offset(address));
                p = *p_addr;
        }

        if (!p) {  // no page
                p = page_alloc(ram->init_val);
                *p_addr = p;
        }

        return p;
}

void
ASL_ram_init(int64_t address_size, int64_t size, ASL_ram_t *ram, uint64_t val)
{
        table_free(ram->p, 1);
        ram->p = 0;
        ram->init_val = (uint8_t)val;
}

uint64_t
ASL_ram_read(int64_t address_size, int64_t size, ASL_ram_t *ram,
             uint64_t address)
{
        runtime_error_if(size > 8, "unsupported read access size");

        uint64_t val = 0;
        for (int i = 0; i < size; ++i) {
                uint8_t *page = page_lookup(ram, address + i);
                *((uint8_t *)&val + i) = *(page + page_offset(address + i));
        }
        return val;
}

void
ASL_ram_write(int64_t address_size, int64_t size, ASL_ram_t *ram,
              uint64_t address, uint64_t val)
{
        runtime_error_if(size > 8, "unsupported write access size");

        for (int i = 0; i < size; ++i) {
                uint8_t *page = page_lookup(ram, address + i);
                *(page + page_offset(address + i)) = *((uint8_t *)&val + i);
        }
}

////////////////////////////////////////////////////////////////
// End
////////////////////////////////////////////////////////////////
