import { Breadcrumbs as MUIBreadcrumbs, Link } from '@material-ui/core';
import NavigateNextIcon from '@material-ui/icons/NavigateNext';
import isMobile from 'is-mobile';
import React from 'react';

interface Breadcrumb {
  title: string;
  url: string;
}

interface BreadcrumbsProps {
  links: Breadcrumb[];
}

const Breadcrumbs: React.FC<BreadcrumbsProps> = ({ links }) => {
  const isMobileBrowser = isMobile();
  const maxItems = isMobileBrowser ? 2 : 4;
  return (
    <>
      <MUIBreadcrumbs maxItems={maxItems} separator={<NavigateNextIcon fontSize="small" />} style={{ marginBottom: '10px' }}>
        <Link color="inherit" href="/">
          Home
        </Link>
        {links.map((link, index) => (
          <Link color={index === links.length - 1 ? 'textPrimary' : 'inherit'} href={link.url} key={index}>
            {link.title}
          </Link>
        ))}
      </MUIBreadcrumbs>
    </>
  );
};

export default Breadcrumbs;
