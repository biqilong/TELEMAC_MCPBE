#!/usr/bin/python3
"""
Contains functions to plot 1d graphics
"""

def plot1d(axe, x, data, plot_label='',
           x_label='', y_label='', **kwargs):
    """
    plot data in 1d

    @param axe (matplotlib.axes3d) matplotlib axes on which to draw
    @param x (numpy.array) List of x 
    @param data (numpy.array) List of values
    @param plot_label (string) Name of the plot (default '')
    @param x_label (string) Name of the x_label (default '')
    @param y_label (string) Name of the y_label (default '')
    @param kwargs (dict) rest of optional arguments given to plot
    """
    # Plot data
    axe.plot(x, data, label=plot_label, **kwargs)

    # Set default axis names
    axe.set_xlabel(x_label)
    axe.set_ylabel(y_label)


#def plot1d_history(axe, times, values, plot_label='',
#                   x_label='', y_label='', **kwargs):
#    """
#    plot a simple evolution over time

#    @param axe (matplotlib.axes3d) matplotlib axes on which to draw
#    @param times (numpy.array) List of time value
#    @param values (numpy.array) List of values
#    @param plot_label (string) Name of the plot (default '')
#    @param x_label (string) Name of the x_label (default '')
#    @param y_label (string) Name of the y_label (default '')
#    @param kwargs (dict) rest of optional arguments given to plot
#    """
#    axe.plot(times, values, label=plot_label, **kwargs)

#    # Set default axis names
#    axe.set_xlabel(x_label)
#    axe.set_ylabel(y_label)

#def plot1d_polyline(axe, x, values, plot_label='',
#                    x_label='', y_label='', **kwargs):
#    """
#    plot a simple evolution along a polyline

#    @param axe (matplotlib.axes3d) matplotlib axes on which to draw
#    @param x (numpy.array) x values
#    @param plot_label (string) Name of the plot (default '')
#    @param x_label (string) Name of the x_label (default '')
#    @param y_label (string) Name of the y_label (default '')
#    @param kwargs (dict) rest of optional arguments given to plot
#    """
#    axe.plot(x, values, label=plot_label, **kwargs)

#    # Set default axis names
#    axe.set_xlabel(x_label)
#    axe.set_ylabel(y_label)
