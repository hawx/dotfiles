# -*- coding: utf-8 -*-
require 'fileutils'
require 'pathname'
require 'taglib'

# For tag names see:
# - http://help.mp3tag.de/main_tags.html
# - https://code.google.com/p/mp4v2/wiki/iTunesMetadata

def song_disc(song)
  case File.extname(song)
  when '.mp3'
    return TagLib::MPEG::File.open(song) do |file|
      tag = file.id3v2_tag
      disc = tag.frame_list("TPOS").first
      unless disc.nil?
        d, ds = tag.frame_list("TPOS").map(&:to_string).map(&:to_i)
        d if (ds && ds > 1) || d > 1
      end
    end
  when '.m4a'
    return TagLib::MP4::File.open(song) do |file|
      map = file.tag.item_list_map
      disc = map['disk']
      unless disc.nil?
        d, ds = disc.to_int_pair
        d if (ds && ds > 1) || d > 1
      end
    end
  end
end

def song_album_artist(song)
  case File.extname(song)
  when '.mp3'
    TagLib::MPEG::File.open(song) do |file|
      tag = file.id3v2_tag
      a = tag.frame_list("TPE1").map(&:to_string)
      b = tag.frame_list("TPE2").map(&:to_string)

      return b.first unless b.empty?
      return a.first unless a.empty?
    end
  when '.m4a'
    TagLib::MP4::File.open(song) do |file|
      map = file.tag.item_list_map

      b = map['aART']
      if b
        b = b.to_string_list
        return b.first unless b.empty?
      end

      a = map['©ART']
      if a
        a = a.to_string_list
        return a.first unless a.empty?
      end
    end
  end
end

def song_track(song, track)
  if track.zero?
    return ""
  end

  disc = song_disc(song)
  if disc
    track = "%d-%02d " % [disc, track]
  else
    track = "%02d " % [track]
  end
end

def sanitise(name)
  name.gsub /[\/;:\*\?"]/, '_'
end

def path_for_song(root, song)
  artist = "Unknown Artist"
  album = "Unknown Album"
  track = ""

  TagLib::FileRef.open(song) do |ref|
    unless ref.null?
      tag = ref.tag

      artist = sanitise(song_album_artist(song) || tag.artist || artist)
      album = sanitise(tag.album || album)

      _track = song_track(song, tag.track)
      track = "%s%s%s" % [_track, sanitise(tag.title || "Unknown Title"), File.extname(song)]
    end
  end

  root + artist + album + track
end
