import ListItem from '@material-ui/core/ListItem';
import ListItemIcon from '@material-ui/core/ListItemIcon';
import ListItemText from '@material-ui/core/ListItemText';
import BarChartIcon from '@material-ui/icons/BarChart';
import DashboardIcon from '@material-ui/icons/Dashboard';
import FavoriteIcon from '@material-ui/icons/Favorite';
import HomeIcon from '@material-ui/icons/Home';
import LibraryIcon from '@material-ui/icons/ImportContacts';
import { useRouter } from 'next/router';
import Link from '../Link';

const SidenavItems: React.FC = () => {
  const router = useRouter();
  const currentPath = router.pathname;

  return (
    <div>
      <ListItem button component={Link} href="/" color="inherit" selected={currentPath === '/'}>
        <ListItemIcon>
          <HomeIcon />
        </ListItemIcon>
        <ListItemText primary="Home" />
      </ListItem>
      <ListItem button component={Link} href="/browse" color="inherit" selected={currentPath === '/browse'}>
        <ListItemIcon>
          <DashboardIcon />
        </ListItemIcon>
        <ListItemText primary="Browse" />
      </ListItem>
      <ListItem button>
        <ListItemIcon>
          <LibraryIcon />
        </ListItemIcon>
        <ListItemText primary="Collection" />
      </ListItem>
      <ListItem button>
        <ListItemIcon>
          <BarChartIcon />
        </ListItemIcon>
        <ListItemText primary="Goals" />
      </ListItem>
      <ListItem button>
        <ListItemIcon>
          <FavoriteIcon />
        </ListItemIcon>
        <ListItemText primary="Patrons" />
      </ListItem>
    </div>
  );
};

export default SidenavItems;
