import ListItem from '@material-ui/core/ListItem';
import ListItemIcon from '@material-ui/core/ListItemIcon';
import ListItemText from '@material-ui/core/ListItemText';
import BarChartIcon from '@material-ui/icons/BarChart';
import DashboardIcon from '@material-ui/icons/Dashboard';
import FavoriteIcon from '@material-ui/icons/Favorite';
import HomeIcon from '@material-ui/icons/Home';
import LibraryIcon from '@material-ui/icons/ImportContacts';
import ListAltIcon from '@material-ui/icons/ListAlt';
import { useRouter } from 'next/router';
import { useAuthentication } from '../../auth/AuthenticationProvider';
import Link from '../Link';

const SidenavItems: React.FC = () => {
  const router = useRouter();
  const { isAuthenticated, user } = useAuthentication();
  const currentPath = router.asPath;

  return (
    <div>
      <ListItem button component={Link} href="/" color="inherit" selected={currentPath === '/'}>
        <ListItemIcon>
          <HomeIcon />
        </ListItemIcon>
        <ListItemText primary="Home" />
      </ListItem>
      <ListItem button component={Link} href="/browse" color="inherit" selected={currentPath.startsWith('/browse')}>
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
        >
          <ListItemIcon>
            <LibraryIcon />
          </ListItemIcon>
          <ListItemText primary="Collection" />
        </ListItem>
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
      <ListItem button component={Link} href="/changelog" color="inherit" selected={currentPath === '/changelog'}>
        <ListItemIcon>
          <ListAltIcon />
        </ListItemIcon>
        <ListItemText primary="Changelog" />
      </ListItem>
    </div>
  );
};

export default SidenavItems;
