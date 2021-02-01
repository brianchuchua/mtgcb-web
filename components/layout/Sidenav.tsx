import styled from 'styled-components';
import Drawer from '@material-ui/core/Drawer';
import { drawerWidth } from './constants';

const Sidenav = styled(Drawer)(({ theme, open }) => {
  if (open) {
    return {
      position: 'relative',
      whiteSpace: 'nowrap',
      width: drawerWidth,
      height: '100vh',
      transition: theme.transitions.create('width', {
        easing: theme.transitions.easing.sharp,
        duration: '500ms',
      }),
      '& .MuiDrawer-paper': {
        position: 'relative',
        whiteSpace: 'nowrap',
        width: drawerWidth,
        height: '100vh',
        transition: theme.transitions.create('width', {
          easing: theme.transitions.easing.sharp,
          duration: '500ms',
        }),
      },
    };
  }

  return {
    overflowX: 'hidden',
    transition: theme.transitions.create('width', {
      easing: theme.transitions.easing.sharp,
      duration: '500ms',
    }),
    width: 0,
    border: 0,
    '& .MuiDrawer-paper': {
      overflowX: 'hidden',
      transition: theme.transitions.create('width', {
        easing: theme.transitions.easing.sharp,
        duration: '500ms',
      }),
      width: 0,
      border: 0,
    },
  };
});

export default Sidenav;
