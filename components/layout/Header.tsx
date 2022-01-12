import Divider from '@material-ui/core/Divider';
import IconButton from '@material-ui/core/IconButton';
import List from '@material-ui/core/List';
import AccountCircleIcon from '@material-ui/icons/AccountCircle';
import ChevronLeftIcon from '@material-ui/icons/ChevronLeft';
import MenuIcon from '@material-ui/icons/Menu';
import { useState } from 'react';
import styled from 'styled-components';
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
  const [isMobileSidenavOpen, setMobileSidenavOpen] = useState(false);

  const handleSidenavOpen = () => {
    setSidenavOpen(true);
    setMobileSidenavOpen(true);
  };
  const handleSidenavClose = () => {
    setSidenavOpen(false);
    setMobileSidenavOpen(false);
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
      <HeaderBarMobile position="fixed" color="inherit" open={isMobileSidenavOpen}>
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
      </HeaderBarMobile>
      <HeaderBarDesktop position="fixed" color="inherit" open={isSidenavOpen}>
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
      </HeaderBarDesktop>
      <SidenavMobile variant="temporary" open={isMobileSidenavOpen} anchor="left" disableScrollLock onClose={handleSidenavClose}>
        <SidenavHeader>
          <IconButton onClick={handleSidenavClose}>
            <ChevronLeftIcon />
          </IconButton>
        </SidenavHeader>
        <Divider />
        <List>
          <SidenavItems handleSidenavClose={handleSidenavClose} />
        </List>
        <Divider />
        <List>
          <SearchForm />
          <SetSearchForm />
          <CollectionSearchForm />
          <SetCollectionSearchForm />
        </List>
      </SidenavMobile>
      <SidenavDesktop variant="permanent" open={isSidenavOpen} anchor="left" disableScrollLock>
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
      </SidenavDesktop>
    </>
  );
};

const SidenavMobile = styled(Sidenav)(({ theme }) => ({
  [theme.breakpoints.up('sm')]: {
    display: 'none',
  },
}));

const SidenavDesktop = styled(Sidenav)(({ theme }) => ({
  [theme.breakpoints.down('xs')]: {
    display: 'none',
  },
}));

const HeaderBarMobile = styled(HeaderBar)(({ theme }) => ({
  [theme.breakpoints.up('sm')]: {
    display: 'none',
  },
}));

const HeaderBarDesktop = styled(HeaderBar)(({ theme }) => ({
  [theme.breakpoints.down('xs')]: {
    display: 'none',
  },
}));

export default Header;
