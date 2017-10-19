import { Set } from 'immutable'

function slugify(text)
{
  return text.toString().toLowerCase()
    .replace(/\s+/g, '-')           // Replace spaces with -
    .replace(/[^\w-]+/g, '')       // Remove all non-word chars
    .replace(/--+/g, '-')         // Replace multiple - with single -
    .replace(/^-+/, '')             // Trim - from start of text
    .replace(/-+$/, '');            // Trim - from end of text
}

export default function mkSlug(text, slugs=Set()) {
  var n = 2;
  const slugified = slugify(text);
  var slug = slugified;
  while( slugs.has(slug) )
    slug = slugified + '-' + n++;
  return slug;
}
