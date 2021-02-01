import styled from 'styled-components';
import AppBar from '@material-ui/core/AppBar';
import { drawerWidth } from './constants';

const HeaderBar = styled(AppBar)(({ theme, open }) => {
  if (open) {
    return {
      marginLeft: drawerWidth,
      width: `calc(100% - ${drawerWidth}px)`,
      transition: theme.transitions.create(['width', 'margin'], {
        easing: theme.transitions.easing.sharp,
        duration: '500ms',
      }),
    };
  }

  return {
    zIndex: theme.zIndex.drawer + 1,
    transition: theme.transitions.create(['width', 'margin'], {
      easing: theme.transitions.easing.sharp,
      duration: '500ms',
    }),
  };
});

export default HeaderBar;
