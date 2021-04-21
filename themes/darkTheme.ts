import { createMuiTheme } from '@material-ui/core/styles';
import { indigo } from '@material-ui/core/colors';

const darkTheme = createMuiTheme({
  palette: {
    type: 'dark',
    primary: { main: '#8a85ff' },
    background: {
      paper: '#282C34',
      default: '#1c2025',
    },
    secondary: indigo,
  },
  overrides: {
    MuiCssBaseline: {
      '@global': {
        '.input-label-fix.MuiInputLabel-outlined.MuiInputLabel-shrink': {
          transform: 'translate(21px, -6px) scale(0.75)', // Fix for Material UI bug with labels on outlined inputs
        },
      },
    },
    MuiPaper: {
      outlined: {
        borderColor: 'rgba(255, 255, 255, 0.23)',
      },
    },
  },
});

export default darkTheme;
