import { useState } from 'react';
import AccountCircleIcon from '@material-ui/icons/AccountCircle';
import ChevronLeftIcon from '@material-ui/icons/ChevronLeft';
import Divider from '@material-ui/core/Divider';
import IconButton from '@material-ui/core/IconButton';
import List from '@material-ui/core/List';
import MenuIcon from '@material-ui/icons/Menu';
import AccountMenu from './AccountMenu';
import HeaderBar from './HeaderBar';
import HeaderToolbar from './HeaderToolbar';
import HeaderSidenavButton from './HeaderSidenavButton';
import HeaderTitle from './HeaderTitle';
import Sidenav from './Sidenav';
import SidenavHeader from './SidenavHeader';
import SidenavItems from './SidenavItems';
import { SearchForm } from '../../features/browse/forms';

const Header: React.FC = () => {
  const [isSidenavOpen, setSidenavOpen] = useState(true);
  const handleSidenavOpen = () => {
    setSidenavOpen(true);
  };
  const handleSidenavClose = () => {
    setSidenavOpen(false);
  };

  const [menuAnchorElement, setMenuAnchorElement] = useState<null | HTMLElement>(null);
  const handleAccountMenuOpen = (event: React.MouseEvent<HTMLButtonElement>) => {
    setMenuAnchorElement(event.currentTarget);
  };
  const handleAccountMenuClose = () => {
    setMenuAnchorElement(null);
  };

  return (
    <>
      <HeaderBar position="fixed" color="inherit" open={isSidenavOpen}>
        <HeaderToolbar>
          <HeaderSidenavButton edge="start" color="inherit" aria-label="Open Side Menu" onClick={handleSidenavOpen}>
            <MenuIcon />
          </HeaderSidenavButton>
          <HeaderTitle component="h1" variant="h6" color="inherit" noWrap>
            MTG Collection Builder
          </HeaderTitle>
          <IconButton color="inherit" aria-controls="account-menu" aria-haspopup="true" onClick={handleAccountMenuOpen}>
            <AccountCircleIcon />
          </IconButton>
          <AccountMenu anchorEl={menuAnchorElement} handleClose={handleAccountMenuClose} />
        </HeaderToolbar>
      </HeaderBar>
      <Sidenav variant="permanent" open={isSidenavOpen} anchor="left">
        <SidenavHeader>
          <IconButton onClick={handleSidenavClose}>
            <ChevronLeftIcon />
          </IconButton>
        </SidenavHeader>
        <Divider />
        <List>
          <SidenavItems />
        </List>
        <Divider />
        <List>
          <SearchForm />
        </List>
      </Sidenav>
    </>
  );
};

export default Header;
