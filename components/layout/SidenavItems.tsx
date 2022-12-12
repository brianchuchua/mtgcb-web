import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemIcon from '@material-ui/core/ListItemIcon';
import ListItemText from '@material-ui/core/ListItemText';
import BarChartIcon from '@material-ui/icons/BarChart';
import DashboardIcon from '@material-ui/icons/Dashboard';
import FavoriteIcon from '@material-ui/icons/Favorite';
import HomeIcon from '@material-ui/icons/Home';
import LibraryIcon from '@material-ui/icons/ImportContacts';
import IsoIcon from '@material-ui/icons/Iso';
import ListAltIcon from '@material-ui/icons/ListAlt';
import StorefrontIcon from '@material-ui/icons/Storefront';
import { useRouter } from 'next/router';
import { useState } from 'react';
import { useAuthentication } from '../../auth/AuthenticationProvider';
import Link from '../Link';

interface SidenavItemsProps {
  handleSidenavClose?: () => void;
}

const SidenavItems: React.FC<SidenavItemsProps> = ({ handleSidenavClose = null }) => {
  const router = useRouter();
  const { isAuthenticated, user } = useAuthentication();
  const currentPath = router.asPath;

  const [isCollectionMenuOpen, setCollectionMenuOpen] = useState(false);

  const handleRouteChange = () => {
    if (handleSidenavClose) {
      handleSidenavClose();
    }
  };
  return (
    <List disablePadding>
      <ListItem button component={Link} href="/" color="inherit" selected={currentPath === '/'} onClick={handleRouteChange}>
        <ListItemIcon>
          <HomeIcon />
        </ListItemIcon>
        <ListItemText primary="Home" />
      </ListItem>
      <ListItem
        button
        component={Link}
        href="/browse"
        color="inherit"
        selected={currentPath.startsWith('/browse')}
        onClick={handleRouteChange}
      >
        <ListItemIcon>
          <DashboardIcon />
        </ListItemIcon>
        <ListItemText primary="Browse" />
      </ListItem>
      {isAuthenticated && (
        <ListItem
          button
          component={Link}
          href={`/collections/${user.id}`}
          color="inherit"
          selected={currentPath?.startsWith(`/collections/${user.id}`)}
          onClick={handleRouteChange}
        >
          <ListItemIcon>
            <LibraryIcon />
          </ListItemIcon>
          <ListItemText primary="Collection" />
        </ListItem>
      )}
      {isAuthenticated && currentPath?.startsWith(`/collections`) && (
        <List component="div" disablePadding>
          <ListItem
            button
            component={Link}
            href="/collections/edit-cards"
            color="inherit"
            selected={currentPath?.startsWith(`/collections/edit-cards`)}
            onClick={handleRouteChange}
          >
            <ListItemIcon style={{ marginLeft: '1.5em' }}>
              <IsoIcon />
            </ListItemIcon>
            <ListItemText primary="Edit Cards" />
          </ListItem>
        </List>
      )}
      <ListItem button>
        <ListItemIcon>
          <BarChartIcon />
        </ListItemIcon>
        <ListItemText primary="Goals (In Development)" />
      </ListItem>
      <ListItem button>
        <ListItemIcon>
          <FavoriteIcon />
        </ListItemIcon>
        <ListItemText primary="Patrons (In Development)" />
      </ListItem>
      <ListItem
        button
        component={Link}
        href="/changelog"
        color="inherit"
        selected={currentPath === '/changelog'}
        onClick={handleRouteChange}
      >
        <ListItemIcon>
          <ListAltIcon />
        </ListItemIcon>
        <ListItemText primary="Changelog" />
      </ListItem>
      <ListItem
        button
        component={Link}
        href="https://cottonbureau.com/products/mtg-collection-builder-apparel-1#/15070134/tee-men-standard-tee-vintage-black-tri-blend-s"
        color="inherit"
        target="_blank"
      >
        <ListItemIcon>
          <StorefrontIcon />
        </ListItemIcon>
        <ListItemText primary="Apparel" />
      </ListItem>
    </List>
  );
};

export default SidenavItems;
