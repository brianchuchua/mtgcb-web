import AppBar from '@material-ui/core/AppBar';
import styled from 'styled-components';
import { drawerWidth } from './constants';

const HeaderBar = styled(AppBar)<HeaderBarProps>(({ theme, open }) => {
  if (open) {
    return {
      marginLeft: drawerWidth,
      width: `calc(100% - ${drawerWidth}px)`,
      transition: theme.transitions.create(['width', 'margin'], {
        easing: theme.transitions.easing.sharp,
        duration: '500ms',
      }),
      zIndex: theme.zIndex.drawer + 1,
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

interface HeaderBarProps {
  theme: any; // eslint-disable-line @typescript-eslint/no-explicit-any
  open: boolean;
}

export default HeaderBar;
