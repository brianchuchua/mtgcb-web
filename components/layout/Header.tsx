import Divider from '@material-ui/core/Divider';
import IconButton from '@material-ui/core/IconButton';
import List from '@material-ui/core/List';
import AccountCircleIcon from '@material-ui/icons/AccountCircle';
import ChevronLeftIcon from '@material-ui/icons/ChevronLeft';
import MenuIcon from '@material-ui/icons/Menu';
import { useState } from 'react';
import { SearchForm } from '../../features/browse/forms';
import CollectionSearchForm from '../../features/collections/forms/CollectionSearchForm';
import { SetCollectionSearchForm } from '../../features/collections/sets/forms';
import { SetSearchForm } from '../../features/sets/forms';
import AccountMenu from './AccountMenu';
import HeaderBar from './HeaderBar';
import HeaderSidenavButton from './HeaderSidenavButton';
import HeaderTitle from './HeaderTitle';
import HeaderToolbar from './HeaderToolbar';
import Sidenav from './Sidenav';
import SidenavHeader from './SidenavHeader';
import SidenavItems from './SidenavItems';

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
          <SetSearchForm />
          <CollectionSearchForm />
          <SetCollectionSearchForm />
        </List>
      </Sidenav>
    </>
  );
};

export default Header;
