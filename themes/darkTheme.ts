import { indigo } from '@material-ui/core/colors';
import { createMuiTheme } from '@material-ui/core/styles';
import breakpoints from './breakpoints';

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
  breakpoints: {
    values: breakpoints,
  },
  overrides: {
    MuiCssBaseline: {
      '@global': {
        '.input-label-fix.MuiInputLabel-outlined.MuiInputLabel-shrink': {
          transform: 'translate(22px, -6px) scale(0.75)', // Fix for Material UI bug with labels on outlined inputs
        },
        '*': {
          'scrollbar-width': '10px',
          'scrollbar-color': '#474c50',
        },
        '*::-webkit-scrollbar': {
          width: '10px',
        },

        '*::-webkit-scrollbar-track': {
          'background-color': 'rgba(202,204,206,0.04)',
        },

        '*::-webkit-scrollbar-thumb': {
          'background-color': '#474c50',
          'border-radius': '10px',
        },
        '*::-webkit-scrollbar-corner': {
          'background-color': '#474c50',
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
