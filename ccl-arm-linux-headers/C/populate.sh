#!/bin/sh
# Note that it may be necessary to patch <sys/procfs.h>, since
# it (mis)uses features not supported by GCC 4.0.  See
# <http://gcc.gnu.org/ml/gcc/2005-01/msg00509.html>
CFLAGS="-D_GNU_SOURCE";export CFLAGS
h-to-ffi.sh /usr/include/linux/ultrasound.h
h-to-ffi.sh /usr/include/linux/can.h
h-to-ffi.sh /usr/include/linux/filter.h
h-to-ffi.sh -include sys/socket.h /usr/include/linux/if_arp.h
h-to-ffi.sh /usr/include/linux/if_arcnet.h
h-to-ffi.sh /usr/include/linux/if_strip.h
h-to-ffi.sh /usr/include/linux/lp.h
h-to-ffi.sh /usr/include/linux/snmp.h
h-to-ffi.sh /usr/include/linux/param.h
h-to-ffi.sh /usr/include/linux/firewire-constants.h
#h-to-ffi.sh /usr/include/linux/signal.h
h-to-ffi.sh -include netinet/in.h /usr/include/linux/netfilter_ipv4.h
h-to-ffi.sh /usr/include/linux/dlm.h
h-to-ffi.sh /usr/include/linux/nfs.h
h-to-ffi.sh /usr/include/linux/flat.h
h-to-ffi.sh /usr/include/linux/kernel.h
h-to-ffi.sh /usr/include/linux/nfs3.h
h-to-ffi.sh /usr/include/linux/atmapi.h
h-to-ffi.sh /usr/include/linux/rfkill.h
h-to-ffi.sh /usr/include/linux/pg.h
h-to-ffi.sh /usr/include/linux/sonypi.h
#h-to-ffi.sh /usr/include/linux/dlm_netlink.h
h-to-ffi.sh /usr/include/linux/posix_types.h
h-to-ffi.sh /usr/include/linux/baycom.h
h-to-ffi.sh /usr/include/linux/mman.h
h-to-ffi.sh /usr/include/linux/fib_rules.h
h-to-ffi.sh /usr/include/linux/usbdevice_fs.h
h-to-ffi.sh -include sys/socket.h /usr/include/linux/route.h
h-to-ffi.sh /usr/include/linux/net_tstamp.h
h-to-ffi.sh /usr/include/linux/keyboard.h
#h-to-ffi.sh /usr/include/linux/coda.h
#h-to-ffi.sh /usr/include/linux/if_tunnel.h
h-to-ffi.sh /usr/include/linux/if_hippi.h
h-to-ffi.sh /usr/include/linux/atmioc.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv4/ipt_CLUSTERIP.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv4/ipt_REJECT.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv4/ipt_realm.h
#h-to-ffi.sh /usr/include/linux/netfilter_ipv4/ip_tables.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv4/ipt_LOG.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv4/ipt_ECN.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv4/ipt_ah.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv4/nf_nat.h
#h-to-ffi.sh /usr/include/linux/netfilter_ipv4/ip_queue.h
#h-to-ffi.sh /usr/include/linux/netfilter_ipv4/ipt_SAME.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv4/ipt_ecn.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv4/ipt_TTL.h
#h-to-ffi.sh /usr/include/linux/netfilter_ipv4/ipt_ULOG.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv4/ipt_addrtype.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv4/ipt_ttl.h
h-to-ffi.sh /usr/include/linux/ipx.h
h-to-ffi.sh /usr/include/linux/isdnif.h
h-to-ffi.sh /usr/include/linux/watchdog.h
h-to-ffi.sh /usr/include/linux/minix_fs.h
h-to-ffi.sh /usr/include/linux/if_addr.h
h-to-ffi.sh /usr/include/linux/atm_he.h
h-to-ffi.sh /usr/include/linux/inet_diag.h
h-to-ffi.sh /usr/include/linux/fb.h
h-to-ffi.sh /usr/include/linux/qnxtypes.h
h-to-ffi.sh /usr/include/linux/sound.h
h-to-ffi.sh /usr/include/linux/mempolicy.h
h-to-ffi.sh /usr/include/linux/ncp.h
h-to-ffi.sh /usr/include/linux/atmppp.h
h-to-ffi.sh /usr/include/linux/phantom.h
#h-to-ffi.sh /usr/include/linux/if_pppox.h
h-to-ffi.sh /usr/include/linux/binfmts.h
#h-to-ffi.sh /usr/include/linux/if_ppp.h
#h-to-ffi.sh /usr/include/linux/llc.h
#h-to-ffi.sh /usr/include/linux/netfilter_arp/arpt_mangle.h
#h-to-ffi.sh /usr/include/linux/netfilter_arp/arp_tables.h
h-to-ffi.sh /usr/include/linux/fd.h
h-to-ffi.sh /usr/include/linux/atmclip.h
h-to-ffi.sh /usr/include/linux/virtio_console.h
h-to-ffi.sh /usr/include/linux/jffs2.h
h-to-ffi.sh /usr/include/linux/tc_act/tc_skbedit.h
h-to-ffi.sh /usr/include/linux/tc_act/tc_ipt.h
h-to-ffi.sh /usr/include/linux/tc_act/tc_mirred.h
h-to-ffi.sh /usr/include/linux/tc_act/tc_pedit.h
h-to-ffi.sh /usr/include/linux/tc_act/tc_gact.h
h-to-ffi.sh /usr/include/linux/tc_act/tc_csum.h
h-to-ffi.sh /usr/include/linux/tc_act/tc_nat.h
h-to-ffi.sh /usr/include/linux/signalfd.h
h-to-ffi.sh /usr/include/linux/nfsacl.h
h-to-ffi.sh /usr/include/linux/cuda.h
h-to-ffi.sh /usr/include/linux/if_link.h
#h-to-ffi.sh /usr/include/linux/ppp-comp.h
h-to-ffi.sh /usr/include/linux/pmu.h
h-to-ffi.sh /usr/include/linux/ipmi.h
h-to-ffi.sh /usr/include/linux/raw.h
h-to-ffi.sh /usr/include/linux/nfs4.h
h-to-ffi.sh /usr/include/linux/netrom.h
h-to-ffi.sh /usr/include/linux/som.h
#h-to-ffi.sh /usr/include/linux/phonet.h
h-to-ffi.sh /usr/include/linux/rose.h
#h-to-ffi.sh /usr/include/linux/if_frad.h
h-to-ffi.sh /usr/include/linux/poll.h
h-to-ffi.sh /usr/include/linux/hidraw.h
h-to-ffi.sh /usr/include/linux/a.out.h
h-to-ffi.sh /usr/include/linux/kdev_t.h
h-to-ffi.sh /usr/include/linux/can/raw.h
h-to-ffi.sh /usr/include/linux/can/error.h
h-to-ffi.sh /usr/include/linux/can/gw.h
#h-to-ffi.sh /usr/include/linux/can/bcm.h
h-to-ffi.sh /usr/include/linux/can/netlink.h
h-to-ffi.sh /usr/include/linux/timex.h
h-to-ffi.sh /usr/include/linux/hiddev.h
#h-to-ffi.sh /usr/include/linux/coda_psdev.h
h-to-ffi.sh /usr/include/linux/ncp_mount.h
h-to-ffi.sh /usr/include/linux/virtio_rng.h
h-to-ffi.sh /usr/include/linux/serio.h
h-to-ffi.sh /usr/include/linux/serial.h
h-to-ffi.sh /usr/include/linux/errqueue.h
h-to-ffi.sh /usr/include/linux/if_addrlabel.h
h-to-ffi.sh /usr/include/linux/stat.h
h-to-ffi.sh /usr/include/linux/tty.h
h-to-ffi.sh /usr/include/linux/pfkeyv2.h
h-to-ffi.sh /usr/include/linux/synclink.h
h-to-ffi.sh /usr/include/linux/serial_reg.h
h-to-ffi.sh /usr/include/linux/sdla.h
h-to-ffi.sh /usr/include/linux/eventpoll.h
h-to-ffi.sh /usr/include/linux/types.h
h-to-ffi.sh /usr/include/linux/version.h
h-to-ffi.sh /usr/include/linux/gen_stats.h
h-to-ffi.sh /usr/include/linux/videodev2.h
h-to-ffi.sh /usr/include/linux/cn_proc.h
#h-to-ffi.sh /usr/include/linux/ipv6_route.h
h-to-ffi.sh /usr/include/linux/byteorder/little_endian.h
h-to-ffi.sh /usr/include/linux/byteorder/big_endian.h
h-to-ffi.sh /usr/include/linux/hid.h
h-to-ffi.sh /usr/include/linux/gigaset_dev.h
h-to-ffi.sh /usr/include/linux/gameport.h
h-to-ffi.sh /usr/include/linux/mmtimer.h
h-to-ffi.sh /usr/include/linux/input.h
h-to-ffi.sh /usr/include/linux/if_bridge.h
h-to-ffi.sh /usr/include/linux/cdrom.h
#h-to-ffi.sh /usr/include/linux/omapfb.h
#h-to-ffi.sh /usr/include/linux/auto_fs.h
h-to-ffi.sh /usr/include/linux/hpet.h
h-to-ffi.sh /usr/include/linux/if_fddi.h
h-to-ffi.sh /usr/include/linux/if_plip.h
h-to-ffi.sh /usr/include/linux/elf-em.h
h-to-ffi.sh /usr/include/linux/nfs_idmap.h
h-to-ffi.sh /usr/include/linux/securebits.h
h-to-ffi.sh /usr/include/linux/blkpg.h
h-to-ffi.sh /usr/include/linux/nfs2.h
h-to-ffi.sh /usr/include/linux/ncp_no.h
h-to-ffi.sh /usr/include/linux/utsname.h
h-to-ffi.sh /usr/include/linux/blk_types.h
#h-to-ffi.sh /usr/include/linux/rds.h
#h-to-ffi.sh /usr/include/linux/agpgart.h
h-to-ffi.sh /usr/include/linux/dlm_plock.h
h-to-ffi.sh -include sys/socket.h /usr/include/linux/if.h
h-to-ffi.sh /usr/include/linux/ext2_fs.h
h-to-ffi.sh /usr/include/linux/kernelcapi.h
h-to-ffi.sh /usr/include/linux/ivtvfb.h
h-to-ffi.sh /usr/include/linux/fcntl.h
h-to-ffi.sh /usr/include/linux/xfrm.h
h-to-ffi.sh /usr/include/linux/if_ec.h
h-to-ffi.sh /usr/include/linux/aio_abi.h
#h-to-ffi.sh /usr/include/linux/atm_zatm.h
#h-to-ffi.sh /usr/include/linux/mroute6.h
h-to-ffi.sh /usr/include/linux/mtio.h
h-to-ffi.sh /usr/include/linux/perf_event.h
h-to-ffi.sh /usr/include/linux/nfs4_mount.h
h-to-ffi.sh /usr/include/linux/hdreg.h
h-to-ffi.sh /usr/include/linux/dm-ioctl.h
h-to-ffi.sh /usr/include/linux/taskstats.h
h-to-ffi.sh /usr/include/linux/capability.h
h-to-ffi.sh /usr/include/linux/tiocl.h
h-to-ffi.sh /usr/include/linux/reiserfs_fs.h
h-to-ffi.sh /usr/include/linux/in.h
h-to-ffi.sh /usr/include/linux/ipc.h
h-to-ffi.sh /usr/include/linux/isdn/capicmd.h
h-to-ffi.sh /usr/include/linux/dvb/dmx.h
h-to-ffi.sh /usr/include/linux/dvb/osd.h
h-to-ffi.sh /usr/include/linux/dvb/ca.h
h-to-ffi.sh /usr/include/linux/dvb/version.h
h-to-ffi.sh /usr/include/linux/dvb/frontend.h
h-to-ffi.sh /usr/include/linux/dvb/video.h
h-to-ffi.sh /usr/include/linux/dvb/net.h
h-to-ffi.sh /usr/include/linux/dvb/audio.h
#h-to-ffi.sh /usr/include/linux/netfilter_bridge.h
h-to-ffi.sh /usr/include/linux/if_packet.h
h-to-ffi.sh /usr/include/linux/if_phonet.h
h-to-ffi.sh /usr/include/linux/generic_serial.h
h-to-ffi.sh /usr/include/linux/cramfs_fs.h
#h-to-ffi.sh /usr/include/linux/ip6_tunnel.h
h-to-ffi.sh /usr/include/linux/if_cablemodem.h
h-to-ffi.sh /usr/include/linux/matroxfb.h
h-to-ffi.sh /usr/include/linux/caif/if_caif.h
h-to-ffi.sh /usr/include/linux/caif/caif_socket.h
h-to-ffi.sh /usr/include/linux/loop.h
h-to-ffi.sh /usr/include/linux/un.h
h-to-ffi.sh /usr/include/linux/if_ltalk.h
h-to-ffi.sh /usr/include/linux/firewire-cdev.h
h-to-ffi.sh /usr/include/linux/mii.h
h-to-ffi.sh /usr/include/linux/nubus.h
h-to-ffi.sh /usr/include/linux/atmlec.h
h-to-ffi.sh /usr/include/linux/pkt_cls.h
h-to-ffi.sh /usr/include/linux/virtio_ring.h
h-to-ffi.sh /usr/include/linux/sockios.h
h-to-ffi.sh /usr/include/linux/if_alg.h
h-to-ffi.sh /usr/include/linux/bfs_fs.h
h-to-ffi.sh /usr/include/linux/const.h
#h-to-ffi.sh /usr/include/linux/netfilter_ipv6/ip6_tables.h
#h-to-ffi.sh /usr/include/linux/netfilter_ipv6/ip6t_rt.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv6/ip6t_opts.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv6/ip6t_REJECT.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv6/ip6t_ipv6header.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv6/ip6t_hl.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv6/ip6t_frag.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv6/ip6t_mh.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv6/ip6t_HL.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv6/ip6t_LOG.h
h-to-ffi.sh /usr/include/linux/netfilter_ipv6/ip6t_ah.h
h-to-ffi.sh /usr/include/linux/major.h
h-to-ffi.sh /usr/include/linux/chio.h
h-to-ffi.sh /usr/include/linux/gfs2_ondisk.h
h-to-ffi.sh /usr/include/linux/veth.h
h-to-ffi.sh /usr/include/linux/stddef.h
h-to-ffi.sh /usr/include/linux/affs_hardblocks.h
h-to-ffi.sh /usr/include/linux/isdn_divertif.h
h-to-ffi.sh /usr/include/linux/spi/spidev.h
h-to-ffi.sh /usr/include/linux/coff.h
h-to-ffi.sh /usr/include/linux/ethtool.h
h-to-ffi.sh /usr/include/linux/ppdev.h
h-to-ffi.sh /usr/include/linux/if_tr.h
h-to-ffi.sh /usr/include/linux/icmpv6.h
h-to-ffi.sh /usr/include/linux/limits.h
h-to-ffi.sh /usr/include/linux/radeonfb.h
h-to-ffi.sh /usr/include/linux/i8k.h
h-to-ffi.sh /usr/include/linux/connector.h
h-to-ffi.sh /usr/include/linux/i2c-dev.h
h-to-ffi.sh /usr/include/linux/nbd.h
h-to-ffi.sh /usr/include/linux/neighbour.h
h-to-ffi.sh /usr/include/linux/isdn_ppp.h
h-to-ffi.sh /usr/include/linux/ip_vs.h
h-to-ffi.sh /usr/include/linux/virtio_net.h
h-to-ffi.sh /usr/include/linux/rtc.h
h-to-ffi.sh /usr/include/linux/wimax.h
h-to-ffi.sh /usr/include/linux/qnx4_fs.h
h-to-ffi.sh /usr/include/linux/if_eql.h
h-to-ffi.sh /usr/include/linux/net_dropmon.h
h-to-ffi.sh /usr/include/linux/cyclades.h
h-to-ffi.sh /usr/include/linux/icmp.h
h-to-ffi.sh /usr/include/linux/virtio_config.h
h-to-ffi.sh /usr/include/linux/isdn.h
h-to-ffi.sh /usr/include/linux/uinput.h
h-to-ffi.sh /usr/include/linux/atm_nicstar.h
h-to-ffi.sh /usr/include/linux/dqblk_xfs.h
h-to-ffi.sh /usr/include/linux/atm_eni.h
h-to-ffi.sh /usr/include/linux/virtio_blk.h
h-to-ffi.sh /usr/include/linux/ivtv.h
h-to-ffi.sh -include sys/socket.h /usr/include/linux/netdevice.h
h-to-ffi.sh /usr/include/linux/termios.h
h-to-ffi.sh /usr/include/linux/kernel-page-flags.h
h-to-ffi.sh /usr/include/linux/hdlc.h
h-to-ffi.sh /usr/include/linux/kd.h
h-to-ffi.sh /usr/include/linux/efs_fs_sb.h
h-to-ffi.sh /usr/include/linux/hysdn_if.h
h-to-ffi.sh /usr/include/linux/parport.h
h-to-ffi.sh /usr/include/linux/irda.h
h-to-ffi.sh /usr/include/linux/romfs_fs.h
h-to-ffi.sh /usr/include/linux/ptrace.h
h-to-ffi.sh /usr/include/linux/if_tun.h
h-to-ffi.sh -include linux/types.h /usr/include/linux/mmc/ioctl.h
h-to-ffi.sh /usr/include/linux/cciss_ioctl.h
h-to-ffi.sh /usr/include/linux/reboot.h
h-to-ffi.sh /usr/include/linux/selinux_netlink.h
h-to-ffi.sh /usr/include/linux/magic.h
h-to-ffi.sh /usr/include/linux/dccp.h
h-to-ffi.sh /usr/include/linux/time.h
h-to-ffi.sh /usr/include/linux/dlmconstants.h
h-to-ffi.sh /usr/include/linux/cgroupstats.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_SECMARK.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_CONNMARK.h
#h-to-ffi.sh /usr/include/linux/netfilter/xt_RATEEST.h
h-to-ffi.sh /usr/include/linux/netfilter/nf_conntrack_common.h
#h-to-ffi.sh /usr/include/linux/netfilter/xt_set.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_helper.h
#h-to-ffi.sh /usr/include/linux/netfilter/xt_ipvs.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_addrtype.h
#h-to-ffi.sh /usr/include/linux/netfilter/xt_sctp.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_CLASSIFY.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_realm.h
#h-to-ffi.sh /usr/include/linux/netfilter/xt_conntrack.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_connmark.h
h-to-ffi.sh /usr/include/linux/netfilter/nfnetlink_conntrack.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_length.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_CHECKSUM.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_state.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_esp.h
#h-to-ffi.sh /usr/include/linux/netfilter/xt_rateest.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_tcpudp.h
#h-to-ffi.sh /usr/include/linux/netfilter/xt_physdev.h
#h-to-ffi.sh /usr/include/linux/netfilter/xt_connlimit.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_owner.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_multiport.h
h-to-ffi.sh /usr/include/linux/netfilter/nfnetlink_queue.h
h-to-ffi.sh /usr/include/linux/netfilter/nfnetlink_log.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_IDLETIMER.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_AUDIT.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_tcpmss.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_cluster.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_comment.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_dscp.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_NFLOG.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_MARK.h
#h-to-ffi.sh /usr/include/linux/netfilter/xt_TPROXY.h
#h-to-ffi.sh /usr/include/linux/netfilter/ipset/ip_set_list.h
#h-to-ffi.sh /usr/include/linux/netfilter/ipset/ip_set_bitmap.h
#h-to-ffi.sh /usr/include/linux/netfilter/ipset/ip_set_hash.h
h-to-ffi.sh /usr/include/linux/netfilter/ipset/ip_set.h
h-to-ffi.sh /usr/include/linux/netfilter/nfnetlink_compat.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_recent.h
#h-to-ffi.sh /usr/include/linux/netfilter/xt_TEE.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_DSCP.h
h-to-ffi.sh /usr/include/linux/netfilter/nf_conntrack_tcp.h
#h-to-ffi.sh /usr/include/linux/netfilter/nf_conntrack_sctp.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_cpu.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_LED.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_dccp.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_time.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_quota.h
#h-to-ffi.sh /usr/include/linux/netfilter/xt_osf.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_NFQUEUE.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_TCPOPTSTRIP.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_CT.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_CONNSECMARK.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_string.h
#h-to-ffi.sh /usr/include/linux/netfilter/xt_policy.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_statistic.h
#h-to-ffi.sh /usr/include/linux/netfilter/xt_iprange.h
#h-to-ffi.sh /usr/include/linux/netfilter/xt_mac.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_socket.h
h-to-ffi.sh /usr/include/linux/netfilter/nf_conntrack_tuple_common.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_mark.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_limit.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_connbytes.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_pkttype.h
h-to-ffi.sh /usr/include/linux/netfilter/x_tables.h
h-to-ffi.sh /usr/include/linux/netfilter/nfnetlink.h
#h-to-ffi.sh /usr/include/linux/netfilter/xt_hashlimit.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_u32.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_devgroup.h
h-to-ffi.sh /usr/include/linux/netfilter/nf_conntrack_ftp.h
h-to-ffi.sh /usr/include/linux/netfilter/xt_TCPMSS.h
h-to-ffi.sh /usr/include/linux/virtio_pci.h
h-to-ffi.sh /usr/include/linux/unistd.h
h-to-ffi.sh /usr/include/linux/falloc.h
#h-to-ffi.sh /usr/include/linux/if_pppol2tp.h
h-to-ffi.sh /usr/include/linux/ax25.h
h-to-ffi.sh /usr/include/linux/fanotify.h
h-to-ffi.sh /usr/include/linux/elf.h
h-to-ffi.sh /usr/include/linux/futex.h
h-to-ffi.sh /usr/include/linux/ppp_defs.h
h-to-ffi.sh /usr/include/linux/atmdev.h
h-to-ffi.sh /usr/include/linux/dcbnl.h
h-to-ffi.sh /usr/include/linux/nfs_fs.h
h-to-ffi.sh /usr/include/linux/msg.h
h-to-ffi.sh /usr/include/linux/adfs_fs.h
h-to-ffi.sh /usr/include/linux/meye.h
h-to-ffi.sh /usr/include/linux/nfs_mount.h
h-to-ffi.sh /usr/include/linux/map_to_7segment.h
h-to-ffi.sh /usr/include/linux/raid/md_u.h
h-to-ffi.sh /usr/include/linux/raid/md_p.h
h-to-ffi.sh /usr/include/linux/quota.h
h-to-ffi.sh /usr/include/linux/bpqether.h
h-to-ffi.sh /usr/include/linux/dlm_device.h
h-to-ffi.sh /usr/include/linux/blktrace_api.h
h-to-ffi.sh /usr/include/linux/in6.h
h-to-ffi.sh /usr/include/linux/uio.h
h-to-ffi.sh /usr/include/linux/udp.h
h-to-ffi.sh /usr/include/linux/vhost.h
h-to-ffi.sh /usr/include/linux/netfilter_bridge/ebt_pkttype.h
h-to-ffi.sh /usr/include/linux/netfilter_bridge/ebt_limit.h
#h-to-ffi.sh /usr/include/linux/netfilter_bridge/ebt_nat.h
#h-to-ffi.sh /usr/include/linux/netfilter_bridge/ebt_arpreply.h
h-to-ffi.sh /usr/include/linux/netfilter_bridge/ebt_among.h
h-to-ffi.sh /usr/include/linux/netfilter_bridge/ebt_vlan.h
h-to-ffi.sh /usr/include/linux/netfilter_bridge/ebt_ip.h
#h-to-ffi.sh /usr/include/linux/netfilter_bridge/ebt_arp.h
#h-to-ffi.sh /usr/include/linux/netfilter_bridge/ebtables.h
h-to-ffi.sh /usr/include/linux/netfilter_bridge/ebt_log.h
h-to-ffi.sh /usr/include/linux/netfilter_bridge/ebt_mark_t.h
h-to-ffi.sh /usr/include/linux/netfilter_bridge/ebt_redirect.h
h-to-ffi.sh /usr/include/linux/netfilter_bridge/ebt_stp.h
h-to-ffi.sh /usr/include/linux/netfilter_bridge/ebt_802_3.h
h-to-ffi.sh /usr/include/linux/netfilter_bridge/ebt_nflog.h
h-to-ffi.sh /usr/include/linux/netfilter_bridge/ebt_mark_m.h
#h-to-ffi.sh /usr/include/linux/netfilter_bridge/ebt_ip6.h
#h-to-ffi.sh /usr/include/linux/netfilter_bridge/ebt_ulog.h
h-to-ffi.sh /usr/include/linux/wanrouter.h
h-to-ffi.sh /usr/include/linux/if_x25.h
h-to-ffi.sh /usr/include/linux/ixjuser.h
h-to-ffi.sh /usr/include/linux/arcfb.h
h-to-ffi.sh /usr/include/linux/atm_tcp.h
h-to-ffi.sh /usr/include/linux/mqueue.h
h-to-ffi.sh /usr/include/linux/sem.h
h-to-ffi.sh /usr/include/linux/vt.h
#h-to-ffi.sh /usr/include/linux/atmbr2684.h
h-to-ffi.sh /usr/include/linux/auxvec.h
h-to-ffi.sh /usr/include/linux/ipsec.h
h-to-ffi.sh /usr/include/linux/fs.h
h-to-ffi.sh /usr/include/linux/string.h
h-to-ffi.sh /usr/include/linux/virtio_9p.h
h-to-ffi.sh /usr/include/linux/cm4000_cs.h
h-to-ffi.sh /usr/include/linux/v4l2-mediabus.h
h-to-ffi.sh /usr/include/linux/ioctl.h
h-to-ffi.sh /usr/include/linux/rtnetlink.h
#h-to-ffi.sh /usr/include/linux/hdlc/ioctl.h
h-to-ffi.sh /usr/include/linux/atmsap.h
h-to-ffi.sh /usr/include/linux/comstats.h
h-to-ffi.sh /usr/include/linux/atm.h
h-to-ffi.sh /usr/include/linux/if_vlan.h
h-to-ffi.sh /usr/include/linux/prctl.h
h-to-ffi.sh /usr/include/linux/if_infiniband.h
#h-to-ffi.sh /usr/include/linux/mroute.h
h-to-ffi.sh /usr/include/linux/n_r3964.h
h-to-ffi.sh /usr/include/linux/udf_fs_i.h
h-to-ffi.sh /usr/include/linux/oom.h
h-to-ffi.sh /usr/include/linux/netlink.h
h-to-ffi.sh /usr/include/linux/msdos_fs.h
h-to-ffi.sh /usr/include/linux/pkt_sched.h
h-to-ffi.sh /usr/include/linux/pci.h
h-to-ffi.sh /usr/include/linux/shm.h
h-to-ffi.sh /usr/include/linux/capi.h
#h-to-ffi.sh /usr/include/linux/omap3isp.h
h-to-ffi.sh /usr/include/linux/if_fc.h
h-to-ffi.sh /usr/include/linux/sunrpc/debug.h
h-to-ffi.sh /usr/include/linux/i2o-dev.h
h-to-ffi.sh -include sys/socket.h /usr/include/linux/wireless.h
#h-to-ffi.sh /usr/include/linux/netfilter.h
h-to-ffi.sh /usr/include/linux/wait.h
h-to-ffi.sh /usr/include/linux/personality.h
h-to-ffi.sh /usr/include/linux/x25.h
h-to-ffi.sh /usr/include/linux/times.h
h-to-ffi.sh /usr/include/linux/igmp.h
h-to-ffi.sh /usr/include/linux/inotify.h
#h-to-ffi.sh /usr/include/linux/if_bonding.h
h-to-ffi.sh /usr/include/linux/media.h
h-to-ffi.sh /usr/include/linux/swab.h
h-to-ffi.sh /usr/include/linux/nvram.h
h-to-ffi.sh /usr/include/linux/nl80211.h
h-to-ffi.sh /usr/include/linux/irqnr.h
h-to-ffi.sh /usr/include/linux/dn.h
h-to-ffi.sh /usr/include/linux/bsg.h
h-to-ffi.sh /usr/include/linux/in_route.h
#h-to-ffi.sh /usr/include/linux/virtio_balloon.h
h-to-ffi.sh /usr/include/linux/random.h
h-to-ffi.sh /usr/include/linux/acct.h
h-to-ffi.sh /usr/include/linux/pps.h
h-to-ffi.sh /usr/include/linux/iso_fs.h
h-to-ffi.sh /usr/include/linux/genetlink.h
#h-to-ffi.sh /usr/include/linux/netfilter_ipv6.h
h-to-ffi.sh /usr/include/linux/fsl_hypervisor.h
h-to-ffi.sh /usr/include/linux/pktcdvd.h
h-to-ffi.sh /usr/include/linux/keyctl.h
h-to-ffi.sh /usr/include/linux/fdreg.h
h-to-ffi.sh /usr/include/linux/atmmpc.h
h-to-ffi.sh /usr/include/linux/ip.h
h-to-ffi.sh /usr/include/linux/edd.h
h-to-ffi.sh /usr/include/linux/nfsd/nfsfh.h
h-to-ffi.sh /usr/include/linux/nfsd/export.h
h-to-ffi.sh /usr/include/linux/nfsd/debug.h
h-to-ffi.sh /usr/include/linux/nfsd/stats.h
h-to-ffi.sh /usr/include/linux/sonet.h
h-to-ffi.sh /usr/include/linux/hdlcdrv.h
h-to-ffi.sh /usr/include/linux/tipc_config.h
h-to-ffi.sh /usr/include/linux/net.h
h-to-ffi.sh /usr/include/linux/wimax/i2400m.h
#h-to-ffi.sh /usr/include/linux/ncp_fs.h
h-to-ffi.sh /usr/include/linux/xattr.h
#h-to-ffi.sh /usr/include/linux/reiserfs_xattr.h
h-to-ffi.sh /usr/include/linux/serial_core.h
h-to-ffi.sh /usr/include/linux/l2tp.h
h-to-ffi.sh /usr/include/linux/soundcard.h
h-to-ffi.sh /usr/include/linux/fuse.h
h-to-ffi.sh /usr/include/linux/v4l2-subdev.h
#h-to-ffi.sh /usr/include/linux/auto_fs4.h
h-to-ffi.sh /usr/include/linux/cciss_defs.h
h-to-ffi.sh /usr/include/linux/toshiba.h
h-to-ffi.sh /usr/include/linux/fiemap.h
h-to-ffi.sh /usr/include/linux/tcp.h
#h-to-ffi.sh /usr/include/linux/scc.h
h-to-ffi.sh /usr/include/linux/tipc.h
h-to-ffi.sh /usr/include/linux/resource.h
h-to-ffi.sh /usr/include/linux/fadvise.h
h-to-ffi.sh /usr/include/linux/sched.h
h-to-ffi.sh /usr/include/linux/virtio_ids.h
h-to-ffi.sh /usr/include/linux/adb.h
h-to-ffi.sh /usr/include/linux/apm_bios.h
h-to-ffi.sh /usr/include/linux/atalk.h
h-to-ffi.sh /usr/include/linux/atm_idt77105.h
#h-to-ffi.sh /usr/include/linux/elf-fdpic.h
h-to-ffi.sh /usr/include/linux/ipv6.h
h-to-ffi.sh /usr/include/linux/usb/cdc.h
h-to-ffi.sh /usr/include/linux/usb/ch9.h
h-to-ffi.sh /usr/include/linux/usb/gadgetfs.h
#h-to-ffi.sh /usr/include/linux/usb/ch11.h
h-to-ffi.sh /usr/include/linux/usb/g_printer.h
h-to-ffi.sh /usr/include/linux/usb/video.h
h-to-ffi.sh /usr/include/linux/usb/tmc.h
h-to-ffi.sh /usr/include/linux/usb/audio.h
h-to-ffi.sh /usr/include/linux/usb/midi.h
h-to-ffi.sh /usr/include/linux/usb/functionfs.h
h-to-ffi.sh /usr/include/linux/if_ether.h
h-to-ffi.sh /usr/include/linux/atmarp.h
h-to-ffi.sh /usr/include/linux/atmsvc.h
h-to-ffi.sh /usr/include/linux/tc_ematch/tc_em_meta.h
h-to-ffi.sh /usr/include/linux/tc_ematch/tc_em_text.h
h-to-ffi.sh /usr/include/linux/tc_ematch/tc_em_nbyte.h
h-to-ffi.sh /usr/include/linux/tc_ematch/tc_em_cmp.h
h-to-ffi.sh /usr/include/linux/errno.h
h-to-ffi.sh -include sys/types.h /usr/include/linux/sysctl.h
h-to-ffi.sh /usr/include/linux/cycx_cfm.h
h-to-ffi.sh /usr/include/linux/i2c.h
h-to-ffi.sh /usr/include/linux/socket.h
h-to-ffi.sh /usr/include/linux/ptp_clock.h
h-to-ffi.sh /usr/include/linux/ipmi_msgdefs.h
#h-to-ffi.sh /usr/include/linux/elfcore.h
#h-to-ffi.sh /usr/include/linux/netfilter_decnet.h
#h-to-ffi.sh -include sys/types.h /usr/include/linux/dm-log-userspace.h
h-to-ffi.sh /usr/include/linux/uvcvideo.h
h-to-ffi.sh /usr/include/linux/b1lli.h
h-to-ffi.sh /usr/include/linux/telephony.h
h-to-ffi.sh /usr/include/linux/joystick.h
h-to-ffi.sh /usr/include/linux/pci_regs.h
h-to-ffi.sh /usr/include/linux/cdk.h
h-to-ffi.sh /usr/include/linux/if_slip.h
h-to-ffi.sh /usr/include/linux/screen_info.h
h-to-ffi.sh -include sys/types.h -include netinet/in.h /usr/include/linux/netfilter_arp.h
h-to-ffi.sh /usr/include/linux/suspend_ioctls.h
h-to-ffi.sh /usr/include/linux/utime.h
h-to-ffi.sh /usr/include/linux/audit.h
