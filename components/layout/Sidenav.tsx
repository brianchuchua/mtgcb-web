import Drawer from '@material-ui/core/Drawer';
import styled from 'styled-components';
import { drawerWidth } from './constants';

const Sidenav = styled(Drawer)(({ theme, open }) => {
  if (open) {
    return {
      whiteSpace: 'nowrap',
      width: drawerWidth,
      height: '100vh',
      flexShrink: 0,
      transition: theme.transitions.create('width', {
        easing: theme.transitions.easing.sharp,
        duration: '500ms',
      }),
      '& .MuiDrawer-paper': {
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
    flexShrink: 0,
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
