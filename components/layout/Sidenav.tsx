import Drawer from '@material-ui/core/Drawer';
import styled from 'styled-components';
import { drawerWidth } from './constants';

const Sidenav = styled(Drawer)(({ theme, open }) => {
  if (open) {
    return {
      whiteSpace: 'nowrap',
      border: 0,
      width: drawerWidth,
      height: '100vh',
      flexShrink: 0,
      '& .MuiDrawer-paper': {
        whiteSpace: 'nowrap',
        width: drawerWidth,
        height: '100vh',
      },
    };
  }

  return {
    overflowX: 'hidden',
    width: 0,
    border: 0,
    flexShrink: 0,
    '& .MuiDrawer-paper': {
      overflowX: 'hidden',
      width: 0,
      border: 0,
    },
  };
});

export default Sidenav;
